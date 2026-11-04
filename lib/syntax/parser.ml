open Ast

type t =
  | Lexer_error of { pos : int; msg : string }
  | Parser_error of { start : int; stop : int; msg : string }

module Arith = struct
  let all = [ ("mod", Mod) ]
  let of_string op = List.assoc_opt (String.lowercase_ascii op) all
  let is_arith op = Option.is_some @@ of_string op
end

module Operator = struct
  let all =
    [
      ("eq", Eq);
      ("neq", Neq);
      ("gt", Gt);
      ("gte", Gte);
      ("lt", Lt);
      ("lte", Lte);
      ("like", Like);
      ("is", Is);
      ("in", In);
    ]

  let of_string op = List.assoc_opt (String.lowercase_ascii op) all
  let is_operator op = Option.is_some @@ of_string op
end

module Cardinality = struct
  let all = [ ("one", One); ("maybe", Maybe) ]
  let of_string s = List.assoc_opt (String.lowercase_ascii s) all
end

module Keyword = struct
  type t =
    | Select
    | Create
    | Insert
    | Update
    | Drop
    | Delete
    | Table
    | Database
    | Get
    | Where
    | Order
    | Limit
    | In
    | Set
    | As
    | Asc
    | Desc
    | Or
    | And
    | Not
    | Null

  let all =
    [
      ("select", Select);
      ("s", Select);
      ("create", Create);
      ("c", Create);
      ("insert", Insert);
      ("i", Insert);
      ("update", Update);
      ("u", Update);
      ("delete", Delete);
      ("d", Delete);
      ("drop", Drop);
      ("dp", Drop);
      ("table", Table);
      ("t", Table);
      ("database", Database);
      ("db", Database);
      ("where", Where);
      ("w", Where);
      ("get", Get);
      ("g", Get);
      ("and", And);
      ("or", Or);
      ("not", Not);
      ("order", Order);
      ("o", Order);
      ("limit", Limit);
      ("l", Limit);
      ("in", In);
      ("set", Set);
      ("as", As);
      ("asc", Asc);
      ("desc", Desc);
      ("null", Null);
    ]

  let to_list = all |> List.map (fun (a, _) -> a)

  let to_string k =
    List.find_map (fun (name, k') -> if k' = k then Some name else None) all
    |> Option.get

  let of_string s = List.assoc_opt (String.lowercase_ascii s) all
  let is_keyword s = Option.is_some (of_string s)

  let cluster s =
    let n = String.length s in
    if n < 2 || is_keyword s then None
    else
      let rec split i =
        if i = n then Some []
        else
          let rec try_len len =
            if len = 0 then None
            else
              let w = String.sub s i len in
              match if is_keyword w then split (i + len) else None with
              | Some rest -> Some (w :: rest)
              | None -> try_len (len - 1)
          in
          try_len (n - i)
      in
      split 0
end

type role =
  | Command
  | Target
  | Name
  | Modifier
  | Column
  | Operator
  | Literal
  | Number
  | Type
  | Punct

type token =
  | Ident of string
  | String of string
  | Int of int
  | Colon
  | Comma
  | Semicolon
  | L_paren
  | R_paren
  | Dot
  | Arith_Op of Ast.arith_op
  | Op of Ast.op
  | Eof
  | Error

let string_of_token = function
  | Ident s -> Printf.sprintf "Ident %S" s
  | String s -> Printf.sprintf "String %S" s
  | Int i -> Printf.sprintf "Int %d" i
  | Colon -> "Colon"
  | Comma -> "Comma"
  | L_paren -> "left parent"
  | R_paren -> "right parent"
  | Arith_Op _ -> "arith op"
  | Semicolon -> "Semicolon"
  | Dot -> "dot"
  | Op _ -> "Op"
  | Eof -> "Eof"
  | Error -> "Error"

type spec = { tok : token; start : int; stop : int }

exception Syntax_error of t

let lexer_error pos fmt =
  Printf.ksprintf
    (fun msg -> raise (Syntax_error (Lexer_error { pos; msg })))
    fmt

let parser_error (start, stop) fmt =
  Printf.ksprintf
    (fun msg -> raise (Syntax_error (Parser_error { start; stop; msg })))
    fmt

let start_of = function { start; stop } :: _ -> start | [] -> 0
let recorder : (spec -> role -> unit) ref = ref (fun _ _ -> ())
let mark tok role = !recorder tok role

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false
let is_ident_char c = is_ident_start c || is_digit c
let span_of = function { start; stop; _ } :: _ -> (start, stop) | [] -> (0, 0)
let fail toks fmt = parser_error (span_of toks) fmt
let fail_at position fmt = parser_error position fmt

let expect_token expected = function
  | ({ tok; _ } as t) :: rest when tok = expected ->
      mark t Punct;
      rest
  | toks -> fail toks "expected %s" (string_of_token expected)

let red s = "\027[31m" ^ s ^ "\027[0m"
let bold_red s = "\027[1;31m" ^ s ^ "\027[0m"

let format_error input ~start ~stop msg =
  let len = String.length input in
  let start = min (max 0 start) len in
  let width = max 1 (stop - start) in
  let padding =
    String.init start (fun i -> if input.[i] = '\t' then '\t' else ' ')
  in
  let underline = "^" ^ String.make (width - 1) '~' in
  Printf.sprintf "%s\n%s\n%s%s" (bold_red msg) input padding (red underline)

let keyword_lookups : (string -> token option) list =
  [
    (fun s -> Option.map (fun o -> Arith_Op o) (Arith.of_string s));
    (fun s -> Option.map (fun o -> Op o) (Operator.of_string s));
  ]

let classify_keyword id =
  match List.find_map (fun f -> f id) keyword_lookups with
  | Some tok -> tok
  | None -> Ident id

let tokenizer str =
  let n = String.length str in
  let peek pos = if pos < n then Some str.[pos] else None in
  let take_while pred pos =
    let j = ref pos in
    while !j < n && pred str.[!j] do
      incr j
    done;
    (String.sub str pos (!j - pos), !j)
  in
  let read_string start =
    let buf = Buffer.create 16 in
    let rec loop i =
      match peek i with
      | None -> (Buffer.contents buf, i)
      | Some '"' -> (Buffer.contents buf, i + 1)
      | Some '\\' -> (
          match peek (i + 1) with
          | Some c ->
              Buffer.add_char buf c;
              loop (i + 2)
          | None -> lexer_error i "dangling escape")
      | Some c ->
          Buffer.add_char buf c;
          loop (i + 1)
    in
    loop (start + 1)
  in
  let rec go i acc =
    let emit tok next = go next ({ tok; start = i; stop = next } :: acc) in
    match peek i with
    | None -> { tok = Eof; start = n; stop = n } :: acc |> List.rev
    | Some (' ' | '\t' | '\n' | '\r') -> go (i + 1) acc
    | Some ':' -> emit Colon (i + 1)
    | Some ',' -> emit Comma (i + 1)
    | Some ';' -> emit Semicolon (i + 1)
    | Some '(' -> emit L_paren (i + 1)
    | Some '-' -> emit (Arith_Op Sub) (i + 1)
    | Some '+' -> emit (Arith_Op Add) (i + 1)
    | Some '*' -> emit (Arith_Op Mul) (i + 1)
    | Some '/' -> emit (Arith_Op Div) (i + 1)
    | Some '%' -> emit (Arith_Op Mod) (i + 1)
    | Some ')' -> emit R_paren (i + 1)
    | Some '.' -> emit Dot (i + 1)
    | Some '=' -> emit (Op Eq) (i + 1)
    | Some '!' when peek (i + 1) = Some '=' -> emit (Op Neq) (i + 2)
    | Some '>' when peek (i + 1) = Some '=' -> emit (Op Gte) (i + 2)
    | Some '>' -> emit (Op Gt) (i + 1)
    | Some '<' when peek (i + 1) = Some '=' -> emit (Op Lte) (i + 2)
    | Some '<' -> emit (Op Lt) (i + 1)
    | Some '"' ->
        let str, next = read_string i in
        emit (String str) next
    | Some ch when is_ident_start ch ->
        let id, next = take_while is_ident_char i in
        emit (classify_keyword id) next
    | Some c when is_digit c ->
        let digits, next = take_while is_digit i in
        emit (Int (int_of_string digits)) next
    | Some c -> emit Error (i + 1)
  in
  go 0 []

let print_tokens input =
  tokenizer input
  |> List.iter (fun { tok; start; stop } ->
      Printf.printf "%-16s [%d,%d)\n" (string_of_token tok) start stop)

let plural n word = if n = 1 then word else word ^ "s"

let check_arity start columns rhs =
  let nc = List.length columns in
  match rhs with
  | Values values ->
      let nv = List.length values in
      if nc <> nv then
        parser_error start "%d %s but %d %s" nc (plural nc "column") nv
          (plural nv "value")
  | Sub { query = { get; _ }; _ } ->
      let ng = List.length get in
      if nc <> ng && ng <> 0 then
        parser_error start "%d %s but subquery returns %d" nc
          (plural nc "column") ng
  | Null -> ()
  | Rows rows ->
      List.iter
        (fun row ->
          let nv = List.length row in
          if nv <> nc then
            parser_error start "%d %s but a row has %d %s" nc
              (plural nc "column") nv (plural nv "value"))
        rows

let expand_cluster = function
  | { tok = Ident s; start; _ } :: rest -> (
      match Keyword.cluster s with
      | Some pieces ->
          let _, toks =
            List.fold_left
              (fun (off, acc) p ->
                let len = String.length p in
                let tok =
                  {
                    tok = Ident p;
                    start = start + off;
                    stop = start + off + len;
                  }
                in
                (off + len, tok :: acc))
              (0, []) pieces
          in
          Some (List.rev_append toks rest)
      | None -> None)
  | _ -> None

let expand_head toks =
  match expand_cluster toks with Some toks -> toks | None -> toks

let is_keyword (keywords : Keyword.t list) = function
  | { tok = Ident s; _ } ->
      Keyword.of_string s
      |> Option.fold ~none:false ~some:(fun k -> List.mem k keywords)
  | _ -> false

let expect_keyword expected = function
  | token :: rest when is_keyword [ expected ] token ->
      mark token Command;
      rest
  | toks -> fail toks "expected %s" (Keyword.to_string expected)

let parse_name role = function
  | ({ tok = Ident s; _ } as t) :: rest ->
      mark t role;
      (s, rest)
  | _ as toks -> fail toks "expacted a name"

let parse_tuple_of ~what parse_item toks =
  match toks with
  | ({ tok = L_paren; start = open_start } as t) :: rest ->
      mark t Punct;
      let rec loop acc toks =
        let item, rest = parse_item toks in
        match rest with
        | ({ tok = Comma; _ } as c) :: rest ->
            mark c Punct;
            loop (item :: acc) rest
        | ({ tok = R_paren; _ } as rp) :: rest ->
            mark rp Punct;
            (List.rev (item :: acc), rest)
        | toks -> fail toks "unclosed '(' in %s list" what
      in
      loop [] rest
  | _ ->
      let item, rest = parse_item toks in
      ([ item ], rest)

and parse_column_ref = function
  | ({ tok = Ident table } as t)
    :: ({ tok = Dot } as p)
    :: ({ tok = Ident col } as c)
    :: rest ->
      mark t Name;
      mark p Punct;
      mark c Column;
      ({ qualifier = Some table; name = col }, rest)
  | ({ tok = Ident s; _ } as t) :: rest ->
      mark t Column;
      ({ qualifier = None; name = s }, rest)
  | toks -> fail toks "expected a column name"

and parse_value = function
  | ({ tok = Ident s | String s; _ } as t) :: rest ->
      mark t Literal;
      (Ast.String s, rest)
  | ({ tok = Int i; _ } as t) :: rest ->
      mark t Number;
      (Ast.Int i, rest)
  | toks -> fail toks "expected a value"

let parse_type = function
  | ({ tok = Ident s; start; stop } as t) :: rest -> (
      match Types.of_string s with
      | Some typ ->
          mark t Type;
          (typ, rest)
      | None -> fail_at (start, stop) "unknown type %s" s)
  | toks -> fail toks "expected a type"

let parse_arith_op = function
  | ({ tok = Arith_Op op; _ } as t) :: rest ->
      mark t Punct;
      (op, rest)
  | toks -> fail toks "expected an math operator"

let rec parse_expr toks =
  let lhs, rest = parse_term toks in
  parse_expr_rest lhs rest

and parse_expr_rest lhs = function
  | ({ tok = Arith_Op ((Add | Sub) as op) } as t) :: rest ->
      mark t Punct;
      let rhs, rest = parse_term rest in
      parse_term_rest (Arith (op, lhs, rhs)) rest
  | toks -> (lhs, toks)

and parse_term toks =
  let lhs, rest = parse_factorial toks in
  parse_term_rest lhs rest

and parse_term_rest lhs = function
  | ({ tok = Arith_Op ((Div | Mul | Mod) as op) } as t) :: rest ->
      mark t Punct;
      let rhs, rest = parse_factorial rest in
      parse_term_rest (Arith (op, lhs, rhs)) rest
  | toks -> (lhs, toks)

and parse_factorial : spec list -> expr * spec list = function
  | ({ tok = Arith_Op Sub } as t) :: rest ->
      mark t Punct;
      let fact, rest = parse_factorial rest in
      (Neg fact, rest)
  | ({ tok = Arith_Op Add } as t) :: rest ->
      mark t Punct;
      let fact, rest = parse_factorial rest in
      (fact, rest)
  | ({ tok = Int i } as t) :: rest ->
      mark t Number;
      (Literal (Int i), rest)
  | ({ tok = String s } as t) :: rest ->
      mark t Literal;
      (Literal (String s), rest)
  | { tok = Ident _; _ } :: _ as toks ->
      let col_ref, rest = parse_column_ref toks in
      (Column col_ref, rest)
  | toks -> fail toks "failed to parse factorial operation"

let parse_select_item toks =
  let expr, rest = parse_expr toks in
  match rest with
  | { tok = Ident s } :: rest as toks ->
      let rest = expect_keyword As toks in
      let alias, rest = parse_name Name rest in
      ({ expr; alias = Some alias }, rest)
  | _ -> ({ expr; alias = None }, rest)

let parse_target = function
  | ({ tok = Ident word; start; stop } as t) :: rest -> (
      let finish mk =
        mark t Target;
        let name, rest = parse_name Name rest in
        (mk name, rest)
      in
      match word |> Keyword.of_string with
      | Some Table -> finish (fun name -> Table name)
      | Some Database -> finish (fun name -> Database name)
      | _ -> fail_at (start, stop) "expected table or database, got %s" word)
  | toks -> fail toks "expected table or database"

let parse_target_ref toks =
  let table, rest =
    match parse_target toks with
    | Table table, rest -> (table, rest)
    | _, rest -> fail rest "expected an table"
  in
  match rest with
  | ({ tok = Ident s } as t) :: rest when Keyword.of_string s = Some As ->
      mark t Command;
      let alias, rest = parse_name Name rest in
      ({ table; alias = Some alias }, rest)
  | _ -> ({ table; alias = None }, rest)

let parse_columns toks = parse_tuple_of ~what:"column" parse_column_ref toks
let parse_exprs toks = parse_tuple_of ~what:"expression" parse_expr toks
let parse_values toks = parse_tuple_of ~what:"value" parse_value toks
let parse_get toks = parse_tuple_of ~what:"get" parse_select_item toks

let parse_field role parse_item toks =
  let key, toks = parse_name role toks in
  match toks with
  | ({ tok = Colon; _ } as t) :: rest ->
      mark t Punct;
      let value, rest = parse_item rest in
      ((key, value), rest)
  | toks -> fail toks "expected ':'"

let rec parse_fields role parse_item toks =
  match toks with
  | { tok = Ident _; _ } :: rest ->
      let field, toks = parse_field role parse_item toks in
      let fields, toks = parse_fields role parse_item toks in
      (field :: fields, toks)
  | _ -> ([], toks)

let expect_end = function
  | [] | [ { tok = Eof; _ } ] -> ()
  | { start; stop } :: _ -> fail_at (start, stop) "unexpected trailing input"

let parse_op : spec list -> op * spec list = function
  | ({ tok = Op op; _ } as t) :: rest ->
      mark t Operator;
      (op, rest)
  | toks -> fail toks "expected an operator"

let card_of_op : op -> cardinality = function
  | Eq | Gt | Gte | Lt | Lte | Like | Neq | Is -> One
  | In -> Many

let parse_order toks =
  let column, toks = parse_expr toks in
  let dir, toks =
    match toks with
    | ({ tok = Ident s; _ } as t) :: rest -> (
        match Keyword.of_string s with
        | Some Keyword.Desc ->
            mark t Modifier;
            (`Desc, rest)
        | Some Keyword.Asc ->
            mark t Modifier;
            (`Asc, rest)
        | _ -> (`Asc, toks))
    | _ -> (`Asc, toks)
  in
  ({ column; dir }, toks)

let parse_null = function
  | ({ tok = Ident s; _ } as t) :: rest
    when Keyword.of_string s = Some Keyword.Null ->
      mark t Operator;
      (Null, rest)
  | toks -> fail toks "'is expects null"

let is_subquery = function
  | { tok = L_paren; _ } :: rest -> (
      match expand_head rest with
      | { tok = Ident s; _ } :: _ -> Keyword.of_string s = Some Keyword.Select
      | _ -> false)
  | _ -> false

let parse_card (toks : spec list) : cardinality option * spec list =
  match toks with
  | ({ tok = Ident w; _ } as t) :: rest -> (
      match Cardinality.of_string w with
      | Some card ->
          mark t Modifier;
          (Some card, rest)
      | None -> (None, toks))
  | _ -> (None, toks)

let accept_keyword kw = function
  | t :: rest when is_keyword [ kw ] t ->
      mark t Operator;
      Some rest
  | _ -> None

let rec parse_or toks =
  let lhs, toks = parse_and toks in
  match accept_keyword Keyword.Or toks with
  | Some rest ->
      let rhs, rest = parse_or rest in
      (Or (lhs, rhs), rest)
  | None -> (lhs, toks)

and parse_and toks =
  let lhs, toks = parse_not toks in
  match accept_keyword Keyword.And toks with
  | Some rest ->
      let rhs, rest = parse_and rest in
      (And (lhs, rhs), rest)
  | _ -> (lhs, toks)

and parse_not toks =
  match accept_keyword Keyword.Not toks with
  | Some rest ->
      let c, rest = parse_not rest in
      (Not c, rest)
  | None -> parse_where toks

and parse_query table toks =
  let empty_state () =
    { table; get = []; where = None; limit = None; order = None }
  in
  let ensure_once state start fmt =
    if Option.is_some state then parser_error start fmt
  in
  let rec loop state = function
    | ([] | { tok = Eof | R_paren } :: _) as rest -> (state, rest)
    | ({ tok = Ident word; start; stop } as t) :: rest -> (
        match word |> Keyword.of_string with
        | Some Get ->
            mark t Modifier;
            let attributes, rest = parse_get rest in
            loop { state with get = state.get @ attributes } rest
        | Some Where ->
            mark t Modifier;
            ensure_once state.where (start, stop) "duplicate where";
            let condition, rest = parse_or rest in
            loop { state with where = Some condition } rest
        | Some Order ->
            mark t Modifier;
            ensure_once state.order (start, stop) "duplicate order";
            let order, rest = parse_order rest in
            loop { state with order = Some order } rest
        | Some Limit -> (
            mark t Modifier;
            ensure_once state.limit (start, stop) "duplicate limit";
            match rest with
            | { tok = Int i; _ } :: rest when i >= 0 ->
                loop { state with limit = Some i } rest
            | { start; stop } :: _ ->
                fail_at (start, stop) "limit expects a non-negative integer"
            | [] -> fail_at (start, stop) "limit expects a number")
        | _ -> fail_at (start, stop) "unknown modifier %s" word)
    | toks -> fail toks "expected a modifier"
  in
  loop (empty_state ()) toks

and parse_subquery toks =
  (* let tok_pos = span_of toks in *)
  let rest = expect_token L_paren toks in
  let rest = expand_head rest in
  let rest = expect_keyword Select rest in
  let table, rest = parse_target_ref rest in
  (* let table, rest = *)
  (*   match parse_target rest with *)
  (*   | Table table, rest -> (table, rest) *)
  (*   | Database _, _ -> *)
  (*       fail_at tok_pos "expected a table select rather than a database select" *)
  (* in *)
  let query, rest = parse_query table rest in
  (query, expect_token R_paren rest)

and parse_rhs ~ncols op toks =
  match op with
  | Is -> parse_null toks
  | In -> parse_in ~ncols toks
  | _ -> (
      let kw_pos = span_of toks in
      let card, toks = parse_card toks in
      match (card, op, is_subquery toks) with
      | Some _, In, _ ->
          fail_at kw_pos "'in' already accepts any number of rows"
      | Some _, _, false ->
          fail_at kw_pos "'one' and 'maybe' must be followed by a subquery"
      | _, _, true ->
          let query, rest = parse_subquery toks in
          let card = Option.value card ~default:(card_of_op op) in
          (Sub { card; query }, rest)
      | None, _, false ->
          let values, rest = parse_values toks in
          (Values values, rest))

and parse_in ~ncols toks =
  let kw_pos = span_of toks in
  match parse_card toks with
  | Some _, _ -> fail_at kw_pos "'in' already accepts any number of rows"
  | None, _ ->
      if is_subquery toks then
        let query, rest = parse_subquery toks in
        (Sub { card = Many; query }, rest)
      else (
        (match toks with
        | { tok = L_paren; _ } :: _ -> ()
        | _ -> fail_at kw_pos "'in' expects a parenthesized list");
        let parse_row =
          if ncols = 1 then fun t ->
            let v, r = parse_value t in
            ([ v ], r)
          else parse_values
        in
        let rows, rest = parse_tuple_of ~what:"row" parse_row toks in
        (Rows rows, rest))

and parse_where toks =
  let columns, toks = parse_exprs toks in
  let op, toks = parse_op toks in
  let rhs_pos = span_of toks in
  let rhs, toks = parse_rhs ~ncols:(List.length columns) op toks in
  check_arity rhs_pos columns rhs;
  (Cmp { columns; op; rhs }, toks)

let parse_select toks =
  match parse_target toks with
  | Table _, _ ->
      let table, rest = parse_target_ref toks in
      let query, _ = parse_query table rest in
      Select_table query
  | Database db, rest ->
      expect_end rest;
      Select_database db

let parse_create toks =
  match parse_target toks with
  | Table table, rest ->
      let fields, rest = parse_fields Column parse_type rest in
      expect_end rest;
      Create_table { table; fields; fmt = Json }
  | Database name, rest ->
      expect_end rest;
      Create_database name

let parse_insert pos toks =
  match parse_target toks with
  | Table table, rest ->
      let fields, rest = parse_fields Column parse_value rest in
      expect_end rest;
      Insert { table; fields }
  | _ -> parser_error pos "unexpected subject, insert supports only table"

let parse_update pos toks =
  match parse_target toks with
  | Table table, rest ->
      let where, rest =
        match rest with
        | tok :: rest when is_keyword [ Where ] tok ->
            mark tok Modifier;
            let condition, rest = parse_or rest in
            (Some condition, rest)
        | _ -> (None, rest)
      in
      let rest = expect_keyword Set rest in
      let fields, rest = parse_fields Column parse_value rest in
      expect_end rest;
      Update { table; where; fields }
  | _ -> fail_at pos "unexpected subject, update supports only table"

let parse_drop toks =
  let target, rest = parse_target toks in
  expect_end rest;
  Drop target

let parse_delete pos toks =
  match parse_target toks with
  | Table table, rest ->
      let where, rest =
        match rest with
        | tok :: rest when is_keyword [ Where ] tok ->
            mark tok Modifier;
            let condition, rest = parse_or rest in
            (Some condition, rest)
        | [] | [ { tok = Eof } ] -> (None, rest)
        | tok :: _ -> fail rest "expected WHERE or end of statement"
      in
      Delete { table; where }
  | _ -> fail_at pos "unexpected subject, delete supports only table"

let rec parse_command ?(expanded = false) = function
  | ({ tok = Ident s; start; stop } as t) :: rest as toks -> (
      let go f =
        mark t Command;
        f rest
      in
      match Keyword.of_string s with
      | Some Select -> go parse_select
      | Some Create -> go parse_create
      | Some Insert -> go (parse_insert (start, stop))
      | Some Update -> go (parse_update (start, stop))
      | Some Delete -> go (parse_delete (start, stop))
      | Some Drop -> go parse_drop
      | Some _ -> fail_at (start, stop) "'%s' can't start a command" s
      | None when not expanded -> (
          match expand_cluster toks with
          | Some toks -> parse_command ~expanded:true toks
          | None -> fail_at (start, stop) "unknown command: %s" s)
      | None -> fail_at (start, stop) "unknown command: %s" s)
  | toks -> fail toks "expected a command"

let parse_exn input = parse_command (tokenizer input)

let parse input : (command, string) result =
  try
    let toks = tokenizer input in
    Ok (parse_command toks)
  with
  | Syntax_error (Lexer_error { pos; msg }) ->
      Error (format_error input ~start:pos ~stop:(pos + 1) msg)
  | Syntax_error (Parser_error { start; stop; msg }) ->
      Error (format_error input ~start ~stop msg)

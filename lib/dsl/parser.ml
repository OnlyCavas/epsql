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
    (*explain*)
    | Explain
    | Analize
    (*commands*)
    | Select
    | Create
    | Insert
    | Update
    | Drop
    | Delete
    | Alter
    | Truncate
    (*target*)
    | Table
    | Database
    (*modifiers*)
    | Get
    | Where
    | Order
    | Limit
    | In
    | Or
    | And
    | Not
    (*alter ops*)
    | Add
    | To
    | Rename
    (*keywords*)
    | Set
    | As
    | Asc
    | Desc
    | Null
    (*join*)
    | Join
    | On
    | Inner
    | Full
    | Left
    | Right
    | Cross
    (*truncate*)
    | CASCADE
    | RESTART
    | IDENTITY

  let all =
    [
      (*explain*)
      ("explain", Explain);
      ("ex", Explain);
      ("analize", Analize);
      ("pl", Analize);
      (*commands*)
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
      ("alter", Alter);
      ("a", Alter);
      ("tc", Truncate);
      ("truncate", Truncate);
      (*target*)
      ("table", Table);
      ("t", Table);
      ("database", Database);
      ("db", Database);
      (*modifiers*)
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
      (*alter ops*)
      ("add", Add);
      ("a", Add);
      ("to", To);
      ("rename", Rename);
      ("r", Rename);
      (*keywords*)
      ("set", Set);
      ("as", As);
      ("asc", Asc);
      ("desc", Desc);
      ("null", Null);
      (*join*)
      ("join", Join);
      ("on", On);
      ("j", Join);
      ("i", Inner);
      ("inner", Inner);
      ("f", Full);
      ("full", Full);
      ("l", Left);
      ("left", Left);
      ("r", Right);
      ("right", Right);
      ("c", Cross);
      ("cross", Cross);
      (*truncate*)
      ("cascade", CASCADE);
      ("casc", CASCADE);
      ("restart", RESTART);
      ("rt", RESTART);
      ("id", IDENTITY);
    ]

  let to_list = all |> List.map (fun (a, _) -> a)

  let to_string k =
    all
    |> List.filter_map (fun (name, k') -> if k' = k then Some name else None)
    |> List.sort (fun a b -> compare (String.length b) (String.length a))
    |> List.hd

  let candidates str =
    let str = String.lowercase_ascii str in
    List.filter_map (fun (name, k) -> if name = str then Some k else None) all

  let of_string ?(expect = fun _ -> true) s =
    List.find_opt expect (candidates s)

  let is_keyword s = Option.is_some (of_string s)
  let is_alter_op = function Add | Drop | Rename | To -> true | _ -> false

  let is_join_kind = function
    | Inner | Left | Right | Full | Cross -> true
    | _ -> false

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
  | Float of float
  | Bool of bool
  | Bytes of string
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
  | Float i -> Printf.sprintf "Int %f" i
  | Bool b -> Printf.sprintf "Bool %b" b
  | Bytes b -> Printf.sprintf "Bytes %s" b
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

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

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
    (fun s ->
      match String.lowercase_ascii s with
      | "true" | "t" -> Some (Bool true)
      | "false" | "f" -> Some (Bool false)
      | _ -> None);
    (fun s -> Option.map (fun o -> Arith_Op o) (Arith.of_string s));
    (fun s -> Option.map (fun o -> Op o) (Operator.of_string s));
  ]

let classify_keyword id =
  match List.find_map (fun f -> f id) keyword_lookups with
  | Some tok -> tok
  | None -> Ident id

let bytes_of_hex hex =
  String.init
    (String.length hex / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))

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
    | Some '0' when peek (i + 1) = Some 'x' ->
        let hex, next = take_while is_hex_digit (i + 2) in
        if hex = "" || String.length hex mod 2 <> 0 then emit Error (i + 1)
        else emit (Bytes (hex |> bytes_of_hex)) next
    | Some c when is_digit c -> (
        let digits, next = take_while is_digit i in
        match (peek next, peek (next + 1)) with
        | Some '.', Some d when is_digit d ->
            let frac, next' = take_while is_digit (next + 1) in
            emit (Float (float_of_string (digits ^ "." ^ frac))) next'
        | Some 'f', _ -> emit (Float (float_of_string digits)) (next + 1)
        | _ -> emit (Int (int_of_string digits)) next)
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
  | ({ tok = Bool b } as t) :: rest ->
      mark t Punct;
      (Ast.Bool b, rest)
  | ({ tok = Float f; _ } as t) :: rest ->
      mark t Number;
      (Ast.Float f, rest)
  | ({ tok = Int i; _ } as t) :: rest ->
      mark t Number;
      (Ast.Int i, rest)
  | ({ tok = Bytes b; _ } as t) :: rest ->
      mark t Punct;
      (Ast.Bytes b, rest)
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
      parse_expr_rest (Arith (op, lhs, rhs)) rest
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

let parse_target ?(ignore_table_kw = false) = function
  | { tok = Ident _ } :: _ as toks when ignore_table_kw ->
      let name, rest = parse_name Name toks in
      (Table name, rest)
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

let parse_target_ref ?(ignore_table_kw = false) toks =
  let table, rest =
    match parse_target ~ignore_table_kw toks with
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

let join_kind_of_keyword : Keyword.t -> Ast.join_kind option = function
  | Inner -> Some Ast.Inner
  | Full -> Some Ast.Full
  | Left -> Some Ast.Left
  | Right -> Some Ast.Right
  | Cross -> Some Ast.Cross
  | _ -> None

let parse_join_head : spec list -> Ast.join_kind * spec list = function
  | ({ tok = Ident kind; start; stop } as k)
    :: ({ tok = Ident join } as j)
    :: rest
    when Keyword.of_string join = Some Join -> (
      match
        Option.bind
          (Keyword.of_string ~expect:Keyword.is_join_kind kind)
          join_kind_of_keyword
      with
      | Some kind ->
          mark k Punct;
          mark j Command;
          (kind, rest)
      | None ->
          fail_at (start, stop) "expected a join kind (e.g. inner), got %s" kind
      )
  | ({ tok = Ident join } as j) :: rest when Keyword.of_string join = Some Join
    ->
      mark j Command;
      (Ast.Inner, rest)
  | toks -> fail toks "expected join"

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

and parse_join toks =
  let kind, rest = parse_join_head toks in
  let table, rest = parse_target_ref ~ignore_table_kw:true rest in
  let rest = expect_keyword On rest in
  let condition, rest = parse_or rest in
  ({ kind; table; condition }, rest)

and parse_query table toks =
  let empty_state () =
    { table; joins = []; get = []; where = None; limit = None; order = None }
  in
  let ensure_once state start fmt =
    if Option.is_some state then parser_error start fmt
  in
  let rec loop ?(expand = false) state = function
    | ([] | { tok = Eof | R_paren } :: _) as rest ->
        ({ state with joins = state.joins |> List.rev }, rest)
    | { tok = Ident _ } :: j :: rest as toks when is_keyword [ Join ] j ->
        let join, rest = parse_join toks in
        loop { state with joins = join :: state.joins } rest
    | ({ tok = Ident word; start; stop } as t) :: rest as toks -> (
        match word |> Keyword.of_string with
        | Some Get ->
            mark t Modifier;
            let attributes, rest = parse_get rest in
            loop { state with get = state.get @ attributes } rest
        | Some Join ->
            let join, rest = parse_join toks in
            loop { state with joins = join :: state.joins } rest
        | Some Where ->
            ensure_once state.where (start, stop) "duplicate where";
            mark t Modifier;
            let condition, rest = parse_or rest in
            loop { state with where = Some condition } rest
        | Some Order ->
            ensure_once state.order (start, stop) "duplicate order";
            mark t Modifier;
            let order, rest = parse_order rest in
            loop { state with order = Some order } rest
        | Some Limit -> (
            ensure_once state.limit (start, stop) "duplicate limit";
            mark t Modifier;
            match rest with
            | { tok = Int i; _ } :: rest when i >= 0 ->
                loop { state with limit = Some i } rest
            | { start; stop } :: _ ->
                fail_at (start, stop) "limit expects a non-negative integer"
            | [] -> fail_at (start, stop) "limit expects a number")
        | _ when not expand -> (
            match expand_cluster toks with
            | Some result -> loop ~expand:true state result
            | None -> fail_at (start, stop) "expected a modifier, got (%s)" word
            )
        | _ -> fail_at (start, stop) "unknown modifier %s" word)
    | toks -> fail toks "expected a modifier"
  in
  loop (empty_state ()) toks

and parse_subquery toks =
  let rest = expect_token L_paren toks in
  let rest = expand_head rest in
  let rest = expect_keyword Select rest in
  let table, rest = parse_target_ref rest in
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
          let exprs, rest = parse_exprs toks in
          (Values exprs, rest))

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
            let v, r = parse_expr t in
            ([ v ], r)
          else parse_exprs
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
      expect_end rest;
      Delete { table; where }
  | _ -> fail_at pos "unexpected subject, delete supports only table"

let parse_alter_operation toks =
  let rec loop ?(expand = false) acc = function
    | ([] | { tok = Eof | R_paren } :: _) as rest -> (acc |> List.rev, rest)
    | ({ tok = Ident word; start; stop } as t) :: rest as toks -> (
        match Keyword.of_string ~expect:Keyword.is_alter_op word with
        | Some Rename ->
            mark t Command;
            let acc, rest =
              match rest with
              | t :: ({ tok = Ident table } as c) :: rest
                when is_keyword [ To ] t ->
                  mark t Command;
                  mark c Target;
                  (Rename_Table table :: acc, rest)
              | ({ tok = Ident from_ } as c) :: rest ->
                  mark c Column;
                  let rest = expect_keyword To rest in
                  let to_, rest = parse_name Column rest in
                  (Rename_Column { from_; to_ } :: acc, rest)
              | toks -> fail toks "expected TO or a column name"
            in
            (acc |> List.rev, rest)
        | Some Drop ->
            mark t Command;
            let name, rest = parse_name Column rest in
            loop (Drop_Column name :: acc) rest
        | Some Add ->
            mark t Command;
            let col, rest = parse_field Column parse_type rest in
            loop (Add_Column col :: acc) rest
        | _ when not expand -> (
            match expand_cluster toks with
            | Some cluster -> loop ~expand:true acc cluster
            | None -> fail_at (start, stop) "expected a modifier, got (%s)" word
            )
        | _ -> fail_at (start, stop) "unknown modifier %s" word)
    | toks -> fail toks "expected a modifier"
  in
  loop [] toks

let parse_alter pos toks =
  match parse_target toks with
  | Table table, rest ->
      let ops, rest = parse_alter_operation rest in
      expect_end rest;
      Alter { table; ops }
  | _ -> fail_at pos "alter doesn't support databases"

let parse_truncate pos toks =
  let rec parse_table_targets acc toks =
    let table, rest =
      match parse_target ~ignore_table_kw:true toks with
      | Table table, rest -> (table, rest)
      | _, rest -> fail rest "truncate doesn't support database"
    in
    match rest with
    | ({ tok = Comma } as t) :: rest ->
        mark t Punct;
        parse_table_targets (table :: acc) rest
    | _ -> (table :: acc |> List.rev, rest)
  in
  let tables, rest = parse_table_targets [] toks in
  let ensure_once state start fmt = if state then parser_error start fmt in
  let rec loop state = function
    | ([] | { tok = Eof | R_paren } :: _) as rest -> (state, rest)
    | ({ tok = Ident word; start; stop } as t) :: rest as toks -> (
        match word |> Keyword.of_string with
        | Some CASCADE ->
            ensure_once state.cascade (start, stop) "cascade is already defined";
            mark t Command;
            loop { state with cascade = true } rest
        | Some RESTART ->
            ensure_once state.cascade (start, stop)
              "reset identity is already defined";
            mark t Command;
            let rest = expect_keyword IDENTITY rest in
            loop { state with restart_identity = true } rest
        | _ -> fail toks "expected CASCADE or RESTART IDENTITY")
    | toks -> fail toks "expected CASCADE or RESTART IDENTITY"
  in
  Truncate
    (loop { tables; restart_identity = false; cascade = false } rest |> fst)

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
      | Some Alter -> go (parse_alter (start, stop))
      | Some Truncate -> go (parse_truncate (start, stop))
      | Some Explain -> go parse_explain
      | Some _ -> fail_at (start, stop) "'%s' can't start a command" s
      | None when not expanded -> (
          match expand_cluster toks with
          | Some toks -> parse_command ~expanded:true toks
          | None -> fail_at (start, stop) "unknown command: %s" s)
      | None -> fail_at (start, stop) "unknown command: %s" s)
  | toks -> fail toks "expected a command"

and parse_explain toks =
  let guard = function
    | Select_table _ | Insert _ | Update _ | Delete _ -> true
    | _ -> false
  in
  let analyze, rest =
    match toks with
    | ({ tok = Ident word } as t) :: rest
      when Keyword.of_string word = Some Analize ->
        mark t Punct;
        (true, rest)
    | rest -> (false, rest)
  in
  let command = parse_command rest in
  if not (guard command) then
    fail toks "explain only supports select, insert, update and delete"
  else Explain { analyze; command }

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

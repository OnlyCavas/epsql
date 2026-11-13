module Types = struct
  type t = Auto | String | Int | Bool | Float | Bytes | Time

  let all =
    [
      ("auto", Auto);
      ("string", String);
      ("int", Int);
      ("bool", Bool);
      ("float", Float);
      ("bytes", Bytes);
      ("time", Time);
    ]

  let of_string str = List.assoc_opt (String.lowercase_ascii str) all
  let is_type str = Option.is_some @@ of_string str
end

type arith_op = Add | Sub | Mul | Div | Mod

type value =
  | String of string
  | Int of int
  | Bool of bool
  | Float of float
  | Bytes of string
  | Time of float

type cardinality = One | Maybe | Many
type op = Eq | Neq | Lt | Gt | Lte | Gte | Like | Is | In

type expr =
  | Literal of value
  | Column of column_ref
  | Neg of expr
  | Arith of arith_op * expr * expr

and select_item = { expr : expr; alias : string option }
and column_ref = { qualifier : string option; name : string }

type condition =
  | Cmp of { columns : expr list; op : op; rhs : rhs }
  | And of condition * condition
  | Or of condition * condition
  | Not of condition

and join_kind = Inner | Full | Left | Right | Cross
and join = { kind : join_kind; table : table_ref; condition : condition }

and rhs =
  | Values of expr list
  | Rows of expr list list
  | Sub of { card : cardinality; query : query }
  | Null

and order = { column : expr; dir : [ `Asc | `Desc ] }
and table_ref = { table : string; alias : string option }

and query = {
  table : table_ref;
  get : select_item list;
  joins : join list;
  where : condition option;
  order : order option;
  limit : int option;
}

type fmt = Json | Pretty
type 'a fields = (string * 'a) list
type target = Table of string | Database of string

type alter_op =
  | Add_Column of (string * Types.t)
  | Drop_Column of string
  | Rename_Table of string
  | Rename_Column of { from_ : string; to_ : string }

type truncate = {
  tables : string list;
  restart_identity : bool;
  cascade : bool;
}

type command =
  | Drop of target
  | Create_database of string
  | Create_table of { table : string; fields : Types.t fields; fmt : fmt }
  | Select_database of string
  | Select_table of query
  | Insert of { table : string; fields : value fields }
  | Update of {
      table : string;
      where : condition option;
      fields : value fields;
    }
  | Delete of { table : string; where : condition option }
  | Alter of { table : string; ops : alter_op list }
  | Truncate of truncate
  | Explain of { analyze : bool; command : command }

let type_of_value : value -> Types.t = function
  | String _ -> Types.String
  | Int _ -> Types.Int
  | Bool _ -> Types.Bool
  | Float _ -> Types.Float
  | Bytes _ -> Types.Bytes
  | Time _ -> Types.Time

let show_value = function
  | String str -> str
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | Bytes s -> s
  | Time s -> string_of_float s

let show_fields show_item fields =
  fields
  |> List.map (fun (key, value) ->
      Printf.sprintf "%s:%s" key (value |> show_item))
  |> String.concat ", "

let show_op = function
  | Eq -> "equal"
  | Neq -> "different"
  | Gt -> "greater"
  | Gte -> "greather or equal"
  | Lt -> "lesser"
  | Lte -> "lesser or equal"
  | Like -> "like"
  | Is -> "is"
  | In -> "in"

let show_arith_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

let arith_prec = function Add | Sub -> 1 | Mul | Div | Mod -> 2
let expr_prec = function Arith (op, _, _) -> arith_prec op | _ -> 3

let show_column_ref = function
  | { qualifier = Some q; name } -> q ^ "." ^ name
  | { qualifier = None; name } -> name

let rec show_expr = function
  | Literal l -> l |> show_value
  | Column c -> c |> show_column_ref
  | Neg ((Arith _ | Neg _) as e) -> Printf.sprintf "-(%s)" (show_expr e)
  | Neg e -> "-" ^ show_expr e
  | Arith (op, a, b) ->
      let p = arith_prec op in
      let wrap needs e =
        if needs then Printf.sprintf "(%s)" (show_expr e) else show_expr e
      in
      Printf.sprintf "%s %s %s"
        (wrap (expr_prec a < p) a)
        (show_arith_op op)
        (wrap (expr_prec b <= p) b)

let show_order { column; dir } =
  Printf.sprintf "order column %s by %s" (show_expr column)
    (match dir with `Asc -> "asc" | `Desc -> "desc")

let show_limit limit = Printf.sprintf "limited by %s" (string_of_int limit)
let show_tuple = function [ x ] -> x | xs -> "(" ^ String.concat ", " xs ^ ")"

let show_type = function
  | Types.Auto -> "auto increment"
  | Types.String -> "string"
  | Types.Int -> "int"
  | Types.Bool -> "bool"
  | Types.Float -> "float"
  | Types.Bytes -> "bytes"
  | Types.Time -> "time"

let show_op_phrase op =
  match op with
  | Eq | Neq -> Printf.sprintf "is %s to" (show_op op)
  | Like -> "is like"
  | _ -> Printf.sprintf "is %s than" (show_op op)

let show_card : cardinality -> string = function
  | One -> "exactly one row from"
  | Maybe -> "at most one row from"
  | Many -> "any rows from"

let rec show_query { table; get; where; order; limit } =
  [
    Some (Printf.sprintf "select table %s" (show_target_ref table));
    show_get get;
    Option.map (fun c -> "where " ^ show_condition c) where;
    Option.map show_order order;
    Option.map show_limit limit;
  ]
  |> List.filter_map Fun.id |> String.concat " "

and show_condition = function
  | Cmp { columns; op; rhs } ->
      Printf.sprintf "column %s %s %s"
        (show_tuple (List.map show_expr columns))
        (show_op_phrase op) (show_rhs rhs)
  | And (a, b) ->
      Printf.sprintf "%s and %s" (show_condition a) (show_condition b)
  | Or (a, b) -> Printf.sprintf "%s or %s" (show_condition a) (show_condition b)
  | Not c -> Printf.sprintf "not %s" (show_condition c)

and show_rhs = function
  | Values vs -> "value " ^ show_tuple (List.map show_expr vs)
  | Sub { card; query } ->
      Printf.sprintf "%s (%s)" (show_card card) (show_query query)
  | Null -> "is null"
  | Rows rows ->
      let show_row row = show_tuple (List.map show_expr row) in
      "(" ^ String.concat ", " (List.map show_row rows) ^ ")"

and show_alias show_item alias =
  match alias with Some a -> show_item ^ " as " ^ a | None -> show_item

and show_select_item item = show_alias (show_expr item.expr) item.alias
and show_target_ref item = show_alias item.table item.alias

and show_get = function
  | [] -> None
  | cols ->
      Some
        (Printf.sprintf "get (%s)"
           (String.concat ", " (List.map show_select_item cols)))

let rec string_of_command = function
  | Drop _ -> "drop"
  | Create_database name -> Printf.sprintf "create database with name %s" name
  | Create_table { table; fields; fmt } ->
      Printf.sprintf "create table with name %s (%s)" table
        (show_fields show_type fields)
  | Select_database name -> Printf.sprintf "select database with name %s" name
  | Select_table query -> query |> show_query
  | Insert { table; fields } ->
      Printf.sprintf "insert table with name %s (%s)" table
        (show_fields show_value fields)
  | Update { table; where; fields } ->
      let where =
        match where with Some w -> w |> show_condition | None -> ""
      in
      Printf.sprintf
        "update table with name %s %s update the following fields: (%s)" table
        where
        (show_fields show_value fields)
  | Delete { table; where } ->
      let where =
        match where with Some w -> w |> show_condition | None -> ""
      in
      Printf.sprintf "delete table with name %s %s" table where
  | Alter { table; _ } -> Printf.sprintf "alter table with name %s" table
  | Truncate { tables; restart_identity; cascade } ->
      let flags =
        [ (restart_identity, "restart identity"); (cascade, "cascade") ]
        |> List.filter_map (fun (flag, label) ->
            if flag then Some label else None)
      in
      let suffix =
        match flags with
        | [] -> ""
        | also -> " also " ^ String.concat " and " also
      in
      Printf.sprintf "truncate tables with names (%s)%s"
        (tables |> String.concat ", ")
        suffix
  | Explain { analyze; command } ->
      let analyze = if analyze then "analyze" else "" in
      Printf.sprintf "explain %s %s" analyze (command |> string_of_command)

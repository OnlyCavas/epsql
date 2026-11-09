open Epsql_syntax
open Epsql_syntax.Ast

type stmt = { sql : string; params : Ast.value list }

module type DIALECT = sig
  val placeholder : int -> Ast.value -> string
  val quote_ident : string -> string
  val sql_type : Types.t -> string
end

let quote s = "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""

module Postgres : DIALECT = struct
  let pg_type = function
    | Types.Auto | Types.Int -> "BIGINT"
    | Types.String -> "TEXT"
    | Types.Bool -> "BOOLEAN"
    | Types.Float -> "DOUBLE PRECISION"
    | Types.Bytes -> "BYTEA"
    | Types.Time -> "TIMESTAMPTZ"

  let placeholder n = function
    | Ast.String _ -> Printf.sprintf "$%d" n
    | value -> Printf.sprintf "$%d::%s" n (Ast.type_of_value value |> pg_type)

  let quote_ident = quote
  let sql_type = function Types.Auto -> "BIGSERIAL" | t -> pg_type t
end

module MAKE (D : DIALECT) = struct
  type ctx = { mutable n : int; mutable params : Ast.value list }

  let bind ctx value =
    ctx.n <- ctx.n + 1;
    ctx.params <- value :: ctx.params;
    D.placeholder ctx.n value

  let idents cols = List.map D.quote_ident cols

  let tuple = function
    | [ x ] -> x
    | cols -> "(" ^ String.concat ", " cols ^ ")"

  let op_sql = function
    | Eq -> "="
    | Neq -> "<>"
    | Lt -> "<"
    | Gt -> ">"
    | Lte -> "<="
    | Gte -> ">="
    | Like -> "LIKE"
    | Is -> "IS"
    | In -> "IN"

  let arith_op = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"

  let string_of_kind = function
    | Inner -> "INNER"
    | Full -> "FULL"
    | Left -> "LEFT OUTER"
    | Right -> "RIGHT OUTER"
    | Cross -> "CROSS"

  let rec query ctx { table; get; joins; where; order; limit } =
    let cols =
      match List.map (select_item ctx) get with
      | [] -> "*"
      | entries -> String.concat ", " entries
    in
    let join =
      List.map
        (fun j ->
          Printf.sprintf " %s JOIN %s on %s" (j.kind |> string_of_kind)
            (j.table |> target_ref)
            (condition ctx j.condition))
        joins
      |> String.concat ""
    in
    let base = Printf.sprintf "SELECT %s FROM %s" cols (target_ref table) in
    let where = Option.map (fun cond -> " WHERE " ^ condition ctx cond) where in
    let order =
      Option.map
        (fun { column; dir } ->
          Printf.sprintf " ORDER BY %s %s" (expression ctx column)
            (match dir with `Asc -> "ASC" | `Desc -> "DESC"))
        order
    in
    let limit = Option.map (Printf.sprintf " LIMIT %d") limit in
    let opts value = Option.value value ~default:"" in
    [ base; join; where |> opts; order |> opts; limit |> opts ]
    |> String.concat ""

  and alias name = " AS " ^ D.quote_ident name

  and target_ref tr =
    (tr.table |> D.quote_ident) ^ Option.fold tr.alias ~none:"" ~some:alias

  and select_item ctx si =
    expression ctx si.expr ^ Option.fold si.alias ~none:"" ~some:alias

  and column_ref = function
    | { qualifier = Some q; name } ->
        Printf.sprintf "%s.%s" (q |> D.quote_ident) (name |> D.quote_ident)
    | { qualifier = None; name } -> name |> D.quote_ident

  and expression ctx = function
    | Literal v -> bind ctx v
    | Column col -> col |> column_ref
    | Neg ex -> Printf.sprintf "-(%s)" (expression ctx ex)
    | Arith (op, lhs, rhs) ->
        let lhs = expression ctx lhs in
        let rhs = expression ctx rhs in
        Printf.sprintf "(%s %s %s)" lhs (op |> arith_op) rhs

  and condition ctx = function
    | Cmp { columns; op; rhs } -> (
        let lhs = tuple (List.map (expression ctx) columns) in
        match rhs with
        | Null -> (
            match op with
            | Is | Eq -> lhs ^ " IS NULL"
            | Neq -> lhs ^ " IS NOT NULL"
            | _ -> invalid_arg "NULL only works with Is/Eq/Neq")
        | Values values when op = In ->
            let ps = List.map (expression ctx) values in
            lhs ^ " IN (" ^ String.concat ", " ps ^ ")"
        | Values values ->
            let ps = List.map (expression ctx) values in
            Printf.sprintf "%s %s %s" lhs (op_sql op) (tuple ps)
        | Rows rows ->
            let row r = tuple (List.map (expression ctx) r) in
            lhs ^ " IN (" ^ String.concat ", " (List.map row rows) ^ ")"
        | Sub { card; query = q } -> (
            let sub = query ctx q in
            match card with
            | Many -> Printf.sprintf "%s IN (%s)" lhs sub
            | One | Maybe -> Printf.sprintf "%s %s (%s)" lhs (op_sql op) sub))
    | And (a, b) ->
        let a = condition ctx a in
        let b = condition ctx b in
        Printf.sprintf "(%s AND %s)" a b
    | Or (a, b) ->
        let a = condition ctx a in
        let b = condition ctx b in
        Printf.sprintf "(%s OR %s)" a b
    | Not c -> Printf.sprintf "NOT (%s)" (condition ctx c)

  let to_stmt (cmd : Ast.command) =
    let ctx = { n = 0; params = [] } in
    let finish sql = { sql; params = ctx.params |> List.rev } in
    match cmd with
    | Select_table q -> finish (query ctx q)
    | Select_database database ->
        finish
          "SELECT table_name FROM information_schema.tables WHERE table_schema \
           = 'public'"
    | Delete { table; where } ->
        let w = Option.map (condition ctx) where in
        finish
          (Printf.sprintf "DELETE FROM %s%s" (D.quote_ident table)
             (match w with Some w -> " WHERE " ^ w | _ -> ""))
    | Update { table; where; fields } ->
        let sets =
          List.map (fun (k, v) -> D.quote_ident k ^ "=" ^ bind ctx v) fields
        in
        let w = Option.map (condition ctx) where in
        finish
          (Printf.sprintf "UPDATE %s SET %s%s" (D.quote_ident table)
             (String.concat ", " sets)
             (match w with Some w -> " WHERE " ^ w | _ -> ""))
    | Insert { table; fields } ->
        let cols = idents (List.map fst fields) in
        let ps = List.map (fun (_, y) -> bind ctx y) fields in
        finish
          (Printf.sprintf "INSERT INTO %s (%s) VALUES (%s)"
             (D.quote_ident table) (String.concat ", " cols)
             (String.concat ", " ps))
    | Create_table { table; fields; _ } ->
        let cols =
          List.map (fun (k, v) -> D.quote_ident k ^ " " ^ D.sql_type v) fields
        in
        finish
          (Printf.sprintf "CREATE TABLE %s (%s)" (D.quote_ident table)
             (String.concat ", " cols))
    | Create_database database ->
        finish ("CREATE DATABASE " ^ D.quote_ident database)
    | Drop (Table t) -> finish ("DROP TABLE " ^ D.quote_ident t)
    | Drop (Database t) -> finish ("DROP DATABASE " ^ D.quote_ident t)

  let string_of_stmt { sql } = sql
end

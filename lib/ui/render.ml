module Table = Term.Table
module Result = Epsql_wire.Result
module Message = Epsql_wire.Message

let is_numeric oid =
  List.mem (Int32.of_int oid) [ 20l; 21l; 23l; 26l; 700l; 701l; 1700l ]

let cell (col : Result.column) (value : string option) =
  match value with
  | None -> Table.txt "NULL"
  | Some v when is_numeric col.type_oid -> Table.num v
  | Some v -> Table.txt v

let render (r : Result.t) : string =
  match r.columns with
  | [] -> r.tag
  | cols ->
      let headers = List.map (fun (c : Result.column) -> c.name) cols in
      let rows = List.map (List.map2 cell cols) r.rows in
      Table.render ~headers ~rows
      ^ Printf.sprintf "\n%d rows · %.1f ms" (List.length r.rows) 2.1

let error (e : Message.error) : string = "error: " ^ e.message

let render (r : Result.response) =
  List.iter (fun rs -> print_endline (render rs)) r.result;
  List.iter (fun e -> prerr_endline (error e)) r.errors

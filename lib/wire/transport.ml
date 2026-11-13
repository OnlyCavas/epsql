module Wire = Message

type t = { sock : Unix.file_descr }

let really_read t number =
  let buf = Bytes.create number in
  let rec go offset =
    if offset < number then begin
      let r = Unix.read t.sock buf offset (number - offset) in
      if r = 0 then failwith "connection closed by server";
      go (offset + r)
    end
  in
  go 0;
  buf

let really_write t bytes =
  let n = Bytes.length bytes in
  let rec go offset =
    if offset < n then go (offset + Unix.write t.sock bytes offset (n - offset))
  in
  go 0

let read_message t =
  let header = really_read t 5 in
  let tp = Bytes.get header 0 in
  let len = Int32.to_int (Bytes.get_int32_be header 1) in
  let payload = really_read t (len - 4) in
  (tp, payload)

let rec wait_ready t =
  match Wire.decode (read_message t) with
  | ReadyForQuery _ -> ()
  | ErrorResponse e -> failwith ("startup: " ^ e.message)
  | _ -> wait_ready t

let send t (msg : Wire.front_message) =
  really_write t (Bytes.of_string (Wire.encode msg))

let send_all t msgs =
  really_write t
    (Bytes.of_string (String.concat "" (List.map Wire.encode msgs)))

let send_sql t sql = send t (Query sql)

let send_stmt t sql params =
  send_all t
    [
      Parsed { stmt = ""; query = sql; paramter_types = [] };
      Bind { portal = ""; stmt = ""; params };
      Describe (`Portal, "");
      Execute { portal = ""; max_col = 0 };
      Sync;
    ]

let rec read_response t : Result.response =
  let rec go (cur : Result.t option) acc errs =
    match Wire.decode (t |> read_message) with
    | RowDescription fields ->
        let columns : Result.column list =
          fields
          |> List.map (fun (f : Wire.field) ->
              { Result.name = f.name; type_oid = f.type_iod })
        in
        go (Some { columns; rows = []; tag = "" }) acc errs
    | DataRow cols ->
        go
          (Option.map
             (fun (r : Result.t) -> { r with rows = cols :: r.rows })
             cur)
          acc errs
    | CommandComplete tag ->
        let result =
          match cur with
          | Some r -> { r with rows = r.rows |> List.rev; tag }
          | None -> { columns = []; rows = []; tag }
        in
        go None (result :: acc) []
    | ErrorResponse err -> go cur acc (err :: errs)
    | ReadyForQuery _ ->
        { Result.result = acc |> List.rev; errors = errs |> List.rev }
    | _ -> go cur acc errs
  in
  go None [] []

let create ~host ~port =
  let addr_info =
    match
      Unix.getaddrinfo host (string_of_int port)
        [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
    with
    | addr_info :: _ -> addr_info
    | [] -> failwith ("cannot resolve " ^ host)
  in
  let sock =
    Unix.socket addr_info.ai_family addr_info.ai_socktype addr_info.ai_protocol
  in
  (try Unix.connect sock addr_info.ai_addr
   with e ->
     Unix.close sock;
     raise e);
  { sock }

let connect ~host ~port ~user ~database =
  let t = create ~host ~port in
  send t (Startup { user; database });
  wait_ready t;
  t

let terminate t = send t Terminate

let close t =
  (try terminate t with _ -> ());
  Unix.close t.sock

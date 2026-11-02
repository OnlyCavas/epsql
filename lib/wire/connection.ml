open Wire

let read_message t =
  let header = really_read t 5 in
  let tp = Bytes.get header 0 in
  let len = Int32.to_int (Bytes.get_int32_be header 1) in
  let payload = really_read t (len - 4) in
  (tp, payload)

let rec wait_ready t =
  match decode (read_message t) with
  | ReadyForQuery _ -> ()
  | ErrorResponse e -> failwith ("startup: " ^ e.message)
  | _ -> wait_ready t

let send t (msg : front_message) = really_write t (Bytes.of_string (encode msg))

let send_all t msgs =
  really_write t (Bytes.of_string (String.concat "" (List.map encode msgs)))

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

let rec read_results t =
  match decode (read_message t) with
  | RowDescription fields ->
      fields
      |> List.map (fun f -> f.name)
      |> String.concat " | " |> print_endline;
      read_results t
  | DataRow cols ->
      cols
      |> List.map (Option.value ~default:"NULL")
      |> String.concat " | " |> print_endline;
      read_results t
  | CommandComplete tag ->
      print_endline tag;
      read_results t
  | ErrorResponse e ->
      prerr_endline ("error: " ^ e.message);
      read_results t
  | ReadyForQuery _ -> ()
  | _ -> read_results t

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

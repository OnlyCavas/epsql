open Epsql_syntax
open Epsql
open Epsql_wire
module SqlDialect = Sql.MAKE (Sql.Postgres)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let config_path () =
  let home = Option.value (Sys.getenv_opt "HOME") ~default:"." in
  Filename.concat home ".epsql/config"

let load_config profile =
  let path = config_path () in
  let blocks =
    if Sys.file_exists path then read_file path |> Config.parse else []
  in
  blocks |> Config.lookup profile |> Config.resolve

let run_once cfg input =
  match Parser.parse input with
  | Error msg ->
      prerr_endline msg;
      exit 1
  | Ok cmd ->
      if Unix.isatty Unix.stdout then
        print_endline ("λ " ^ Highlight.colorize input ^ "\n");

      let connection =
        Connection.connect ~host:cfg.Config.host ~port:cfg.port ~user:cfg.user
          ~database:cfg.database
      in
      Fun.protect
        ~finally:(fun () -> Connection.close connection)
        (fun () ->
          Repl.execute (SqlDialect.to_stmt cmd) connection;
          Connection.read_results connection)

let split_profile = function
  | word :: args when String.length word > 1 && word.[0] = '@' ->
      (String.sub word 1 (String.length word - 1), args)
  | args -> ("", args)

let () =
  try
    let profile, args = Sys.argv |> Array.to_list |> List.tl |> split_profile in
    let cfg = load_config profile in
    match args with
    | [] -> Repl.run cfg
    | words -> run_once cfg (words |> String.concat " ")
  with Failure msg ->
    prerr_endline ("epsql: " ^ msg);
    exit 1

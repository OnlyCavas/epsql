module App = Epsql_app
module Dsl = Epsql_dsl
module Ui = Epsql_ui
module Psql = Epsql_wire.Transport
module Config = App.Config
module Repl = App.Repl
module Sql = Epsql_dsl.Sql
module SqlDialect = Sql.MAKE (Sql.Postgres)

let or_fail = function Ok x -> x | Error msg -> failwith msg

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
  let cmd = Dsl.Parser.parse input |> or_fail in

  if Unix.isatty Unix.stdout then
    print_endline ("λ " ^ Ui.Highlight.colorize input);

  let connection =
    Psql.connect ~host:cfg.Config.host ~port:cfg.port ~user:cfg.user
      ~database:cfg.database
  in
  Fun.protect
    ~finally:(fun () -> Psql.close connection)
    (fun () ->
      Repl.execute_sql (SqlDialect.to_stmt cmd) connection;
      connection |> Psql.read_response |> Ui.Render.render)

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

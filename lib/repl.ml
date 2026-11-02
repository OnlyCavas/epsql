open Epsql_syntax
open Epsql_wire
module SqlDilect = Sql.MAKE (Sql.Postgres)

module KeyDec = struct
  type t =
    | Char of char
    | Enter
    | Backspace
    | Delete
    | Left
    | Right
    | Up
    | Down
    | Home
    | End
    | Eof_Key
    | Tab
    | Clear_Screen
    | Ctrl_Backspace
    | Ctrl_Right
    | Ctrl_Left
    | Other

  let read_byte fd bytes =
    if Unix.read fd bytes 0 1 = 0 then raise End_of_file else Bytes.get bytes 0

  let read_key fd bytes =
    match read_byte fd bytes with
    | '\n' -> Enter
    | '\127' -> Backspace
    | '\b' -> Ctrl_Backspace
    | '\023' -> Ctrl_Backspace
    | '\004' -> Eof_Key
    | '\001' -> Home
    | '\005' -> End
    | '\012' -> Clear_Screen
    | '\t' -> Tab
    | '\027' -> (
        match read_byte fd bytes with
        | '[' | 'O' -> (
            match read_byte fd bytes with
            | 'A' -> Up
            | 'B' -> Down
            | 'C' -> Right
            | 'D' -> Left
            | 'H' -> Home
            | 'F' -> End
            | 'c' -> Ctrl_Right
            | 'd' -> Ctrl_Left
            | '1' -> (
                match read_byte fd bytes with
                | ';' -> (
                    let modifier = read_byte fd bytes in
                    match (modifier, read_byte fd bytes) with
                    | '5', 'C' -> Ctrl_Right
                    | '5', 'D' -> Ctrl_Left
                    | _ -> Other)
                | _ -> Other)
            | '3' ->
                ignore (read_byte fd bytes);
                Delete
            | _ -> Other)
        | 'b' -> Ctrl_Left
        | 'f' -> Ctrl_Right
        | _ -> Other)
    | c when c >= ' ' -> Char c
    | _ -> Other
end

module History = struct
  type t = { entries : string list; max : int }

  let empty = { entries = []; max = 500 }
  let nth history idx = List.nth_opt history.entries idx

  let add history line =
    if String.trim line = "" then history
    else
      match history.entries with
      | last :: _ when line = last -> history
      | es ->
          {
            history with
            entries = List.filteri (fun i _ -> i < history.max) (line :: es);
          }

  type navigation = { idx : int; draft : string }

  let start = { idx = -1; draft = "" }

  let prev history nav ~current =
    let idx = nav.idx + 1 in
    match nth history idx with
    | None -> None
    | Some str ->
        let draft = if nav.idx = -1 then current else nav.draft in
        Some (str, { idx; draft })

  let next history nav =
    match nav.idx with
    | -1 -> None
    | 0 -> Some (nav.draft, { nav with idx = -1 })
    | i ->
        Option.map
          (fun s -> (s, { nav with idx = nav.idx - 1 }))
          (nth history (i - 1))
end

let clear_console () = print_string "\027[H\027[2J\027[3J"
let prompt = "λ "
let prompt_with = 2

let is_word_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_'

let word_left l p =
  let i = ref p in
  while !i > 0 && not (is_word_char l.[!i - 1]) do
    decr i
  done;
  while !i > 0 && is_word_char l.[!i - 1] do
    decr i
  done;
  !i

let word_right l p =
  let n = String.length l in
  let i = ref p in
  while !i < n && not (is_word_char l.[!i]) do
    incr i
  done;
  while !i < n && is_word_char l.[!i] do
    incr i
  done;
  !i

let read_statement history =
  let fd = Unix.stdin in
  let old = Unix.tcgetattr fd in
  Unix.tcsetattr fd Unix.TCSADRAIN
    { old with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 };
  Fun.protect ~finally:(fun () -> Unix.tcsetattr fd Unix.TCSADRAIN old)
  @@ fun () ->
  let b = Bytes.create 1 in
  let line = ref "" and pos = ref 0 in
  let nav = ref History.start in
  let redraw () =
    print_string ("\r\027[K" ^ prompt ^ Highlight.colorize !line);
    let col = prompt_with + !pos in
    if col > 0 then Printf.printf "\r\027[%dC" col;
    flush stdout
  in
  let set_line str =
    line := str;
    pos := String.length str
  in
  let insert c =
    let l = !line and p = !pos in
    line :=
      String.sub l 0 p ^ String.make 1 c ^ String.sub l p (String.length l - p);
    incr pos
  in
  let delete_at p =
    let l = !line in
    line := String.sub l 0 p ^ String.sub l (p + 1) (String.length l - p - 1);
    decr pos
  in
  let rec loop () =
    redraw ();
    match KeyDec.read_key fd b with
    | Tab ->
        let l = !line in
        let start =
          match String.rindex_from_opt l (max 0 (!pos - 1)) ' ' with
          | Some i when !pos > 0 -> i + 1
          | _ -> 0
        in
        let prefix = String.sub l start (!pos - start) in
        (match Suggestions.complete prefix Parser.Keyword.to_list with
        | Some word when String.length word > String.length prefix ->
            let rest = String.sub l !pos (String.length l - !pos) in
            line := String.sub l 0 start ^ word ^ rest;
            pos := start + String.length word
        | _ -> ());
        loop ()
    | Enter ->
        print_newline ();
        !line
    | Eof_Key when !line = "" ->
        print_newline ();
        raise End_of_file
    | Eof_Key -> loop ()
    | Char c ->
        insert c;
        loop ()
    | Backspace ->
        if !pos > 0 then delete_at (!pos - 1);
        loop ()
    | Delete ->
        if !pos < String.length !line then delete_at !pos;
        loop ()
    | Ctrl_Backspace ->
        let p = word_left !line !pos in
        line :=
          String.sub !line 0 p
          ^ String.sub !line !pos (String.length !line - !pos);
        pos := p;
        loop ()
    | Ctrl_Left ->
        pos := word_left !line !pos;
        loop ()
    | Left ->
        if !pos > 0 then decr pos;
        loop ()
    | Ctrl_Right ->
        pos := word_right !line !pos;
        loop ()
    | Right ->
        if !pos < String.length !line then incr pos;
        loop ()
    | Home ->
        pos := 0;
        loop ()
    | End ->
        pos := String.length !line;
        loop ()
    | Up ->
        (match History.prev history !nav ~current:!line with
        | Some (draft, n) ->
            set_line draft;
            nav := n
        | None -> ());
        loop ()
    | Down ->
        (match History.next history !nav with
        | Some (draft, n) ->
            set_line draft;
            nav := n
        | None -> ());
        loop ()
    | Clear_Screen ->
        clear_console ();
        loop ()
    | _ -> loop ()
  in
  loop ()

let param_of_value : Ast.value -> string option = function
  | String s -> Some s
  | Int i -> Some (string_of_int i)

let execute (stmt : Sql.stmt) c =
  match stmt.params with
  | [] -> Connection.send_sql c stmt.sql
  | params -> Connection.send_stmt c stmt.sql (List.map param_of_value params)

type state = {
  history : History.t;
  debug : bool;
  connection : Wire.t;
  config : Config.t;
}

let switch_database state database =
  let config = { state.config with Config.database } in
  match
    Connection.connect ~host:config.host ~port:config.port ~user:config.user
      ~database:config.database
  with
  | exception Failure msg ->
      prerr_endline ("could not connect: " ^ msg);
      state
  | connection ->
      Connection.close state.connection;
      { state with connection; config }

let rec run state =
  match read_statement state.history with
  | exception End_of_file ->
      Connection.close state.connection;
      print_newline ()
  | input -> (
      let state = { state with history = History.add state.history input } in
      match Special.classify input with
      | Special Quit ->
          Connection.close state.connection;
          ()
      | Special Debug ->
          let state = { state with debug = not state.debug } in
          Printf.printf "debug %s\n" (if state.debug then "on" else "off");
          run state
      | Special (Use "") ->
          print_endline "usage: @use dbname";
          run state
      | Special (Use database) -> run (switch_database state database)
      | Special Clear ->
          clear_console ();
          run state
      | Special Help ->
          Special.help () |> print_endline;
          run state
      | Unknown name ->
          Printf.printf "unknown command %c%s (try %ch)\n" Special.prefix name
            Special.prefix;
          run state
      | Query q ->
          if state.debug then Parser.print_tokens q;
          (match Parser.parse input with
          | Ok cmd ->
              let stmt = SqlDilect.to_stmt cmd in
              execute stmt state.connection;
              Connection.read_results state.connection
          | Error msg ->
              msg |> print_endline;
              Printf.printf "invalid query %s (try %ch)\n" input Special.prefix);
          run state)

let run cfg =
  run
    {
      history = History.empty;
      debug = false;
      config = cfg;
      connection =
        Connection.connect ~host:cfg.Config.host ~port:cfg.port ~user:cfg.user
          ~database:cfg.database;
    }

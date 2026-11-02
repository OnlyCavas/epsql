type cmd = Quit | Clear | Help | Debug | Use of string

let prefix = '@'

let all : (string * (string -> cmd)) list =
  [
    ("use", fun arg -> Use arg);
    ("quit", fun _ -> Quit);
    ("exit", fun _ -> Quit);
    ("clear", fun _ -> Clear);
    ("help", fun _ -> Help);
    ("debug", fun _ -> Debug);
  ]

let of_string str = List.assoc_opt (String.lowercase_ascii str) all

let describe = function
  | Use _ -> "switch database (@use dbname)"
  | Quit -> "leave the REPL"
  | Clear -> "clear the screen"
  | Debug -> "show debug information"
  | Help -> "show this help"

let help () =
  let line (name, mk) =
    Printf.sprintf "%c%-6s %s" prefix name (describe (mk ""))
  in
  String.concat "\n" (List.map line all)

type input = Special of cmd | Unknown of string | Query of string

let is_partial name =
  let n = String.lowercase_ascii name in
  List.exists (fun (k, _) -> String.starts_with ~prefix:n k) all

let classify line =
  let s = String.trim line in
  if s <> "" && s.[0] = prefix then
    let body = String.sub s 1 (String.length s - 1) in
    let name, arg =
      match String.index_opt body ' ' with
      | None -> (body, "")
      | Some i ->
          ( String.sub body 0 i,
            String.trim (String.sub body i (String.length body - i)) )
    in
    match of_string name with Some c -> Special (c arg) | None -> Unknown name
  else Query line

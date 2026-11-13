type t = {
  host : string;
  port : int;
  password : string;
  user : string;
  database : string;
}

type block = { patterns : string list; opts : (string * string) list }

let show_block b =
  let list f xs = "[" ^ String.concat "; " (List.map f xs) ^ "]" in
  Printf.sprintf "{ patterns = %s; opts = %s }"
    (list (Printf.sprintf "%S") b.patterns)
    (list (fun (k, v) -> Printf.sprintf "(%S, %S)" k v) b.opts)

let show_t t =
  Printf.sprintf
    "{ host = %S; port = %d; user = %S; database = %S; password = %s }" t.host
    t.port t.user t.database
    (if t.password = "" then "<none>" else "<hidden>")

let parse text =
  let finish cur blocks =
    match cur with
    | Some b -> { b with opts = b.opts |> List.rev } :: blocks
    | None -> blocks
  in
  let step (blocks, cur) line =
    let line = String.trim line in
    if line = "" || line.[0] = '#' then (blocks, cur)
    else
      match String.index_opt line ' ' with
      | None -> failwith ("bad config at " ^ line)
      | Some i -> (
          let key = String.lowercase_ascii (String.sub line 0 i) in
          let value =
            String.trim (String.sub line i (String.length line - i))
          in
          if key = "host" then
            ( finish cur blocks,
              Some { patterns = String.split_on_char ' ' value; opts = [] } )
          else
            match cur with
            | Some b -> (blocks, Some { b with opts = (key, value) :: b.opts })
            | None -> failwith "option before any Host line")
  in
  let blocks, cur =
    String.split_on_char '\n' text |> List.fold_left step ([], None)
  in
  finish cur blocks |> List.rev

let find_block needle (blocks : block list) =
  blocks
  |> List.find_opt (fun b ->
      List.exists (fun p -> p = "*" || p = needle) b.patterns)

let get_opt key (block : block) =
  block.opts |> List.assoc_opt @@ String.lowercase_ascii key

let lookup needle (blocks : block list) =
  blocks
  |> List.fold_left
       (fun acc b ->
         if List.exists (fun p -> p = "*" || p = needle) b.patterns then
           b.opts
           |> List.fold_left
                (fun acc (k, v) ->
                  if List.mem_assoc k acc then acc else (k, v) :: acc)
                acc
         else acc)
       []

let resolve (opts : (string * string) list) =
  let get key fallback =
    Option.value (List.assoc_opt key opts) ~default:fallback
  in
  {
    host = get "hostname" "localhost";
    port = int_of_string (get "port" "5432");
    password = get "password" "";
    user = get "user" "postgres";
    database = get "database" "postgres";
  }

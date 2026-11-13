type field = { name : string; type_iod : int }

type error = {
  severity : string;
  code : string;
  message : string;
  position : int option;
}

type front_message =
  | Startup of { user : string; database : string }
  | Query of string
  | Parsed of { stmt : string; query : string; paramter_types : int list }
  | Bind of { portal : string; stmt : string; params : string option list }
  | Describe of [ `PreparedStatement | `Portal ] * string
  | Execute of { portal : string; max_col : int }
  | Sync
  | Terminate

type backend_message =
  | Authentication of int
  | ParameterStatus of string * string
  | BackendKeyData of int * int
  | ReadyForQuery of char
  | RowDescription of field list
  | DataRow of string option list
  | CommandComplete of string
  | ErrorResponse of error
  | NoticeResponse of error
  | Unknown of char * Bytes.t
  (*prepared statements*)
  | ParseComplete
  | BindComplete

let get_cstr ptr pos =
  let last = Bytes.index_from ptr pos '\000' in
  (Bytes.sub_string ptr pos (last - pos), last + 1)

let put_cstr str = str ^ "\000"

let make_int16 n =
  let b = Buffer.create 2 in
  Buffer.add_int16_be b n;
  Buffer.contents b

let make_int32 n =
  let b = Buffer.create 4 in
  Buffer.add_int32_be b (Int32.of_int n);
  Buffer.contents b

let message typ body =
  String.make 1 typ ^ make_int32 (String.length body + 4) ^ body

let encode_string = function
  | None -> make_int32 (-1)
  | Some s -> make_int32 (String.length s) ^ s

let parse_error payload =
  let rec go pos acc =
    if Bytes.get payload pos = '\000' then acc
    else
      let value, next = get_cstr payload (pos + 1) in
      go next ((Bytes.get payload pos, value) :: acc)
  in
  let errors = go 0 [] in
  let get k = List.assoc_opt k errors in
  let get_opt k = Option.value (get k) ~default:"" in
  {
    severity = get_opt 'S';
    code = get_opt 'C';
    message = get_opt 'M';
    position = Option.map int_of_string (get 'P');
  }

let parse_row_description payload =
  let n = Bytes.get_int16_be payload 0 in
  let rec go total pos acc =
    if total = 0 then acc |> List.rev
    else
      let name, next = get_cstr payload pos in
      let type_iod = Bytes.get_int16_be payload (next + 6) in
      go (total - 1) (next + 18) ({ name; type_iod } :: acc)
  in
  go n 2 []

let parse_data_row payload =
  let n = Bytes.get_int16_be payload 0 in
  let rec go total pos acc =
    if total = 0 then acc |> List.rev
    else
      let col_n = Int32.to_int (Bytes.get_int32_be payload pos) in
      if col_n = -1 then go (total - 1) (pos + 4) (None :: acc)
      else
        let value = Bytes.sub_string payload (pos + 4) col_n in
        go (total - 1) (pos + 4 + col_n) (Some value :: acc)
  in
  go n 2 []

let encode : front_message -> string = function
  | Startup { user; database } ->
      let body =
        make_int32 196608 ^ put_cstr "user" ^ put_cstr user
        ^ put_cstr "database" ^ put_cstr database ^ "\000"
      in
      make_int32 (String.length body + 4) ^ body
  | Query sql -> message 'Q' (put_cstr sql)
  | Parsed { stmt; query; paramter_types } ->
      message 'P'
        (put_cstr stmt ^ put_cstr query
        ^ make_int16 (List.length paramter_types)
        ^ String.concat "" (List.map make_int32 paramter_types))
  | Bind { portal; stmt; params } ->
      message 'B'
        (put_cstr portal ^ put_cstr stmt ^ make_int16 0
        ^ make_int16 (List.length params)
        ^ String.concat "" (List.map encode_string params)
        ^ make_int16 0)
  | Describe (kind, name) ->
      message 'D'
        (match kind with
        | `PreparedStatement -> "S"
        | `Portal -> "P" ^ put_cstr name)
  | Execute { portal; max_col } ->
      message 'E' (put_cstr portal ^ make_int32 max_col)
  | Sync -> message 'S' ""
  | Terminate -> message 'X' ""

let decode (typ, payload) =
  let get_int32 payload pos = Int32.to_int (Bytes.get_int32_be payload pos) in
  match typ with
  | 'R' -> Authentication (get_int32 payload 0)
  | 'S' ->
      let name, next = get_cstr payload 0 in
      ParameterStatus (name, fst @@ get_cstr payload next)
  | 'K' -> BackendKeyData (get_int32 payload 0, get_int32 payload 4)
  | 'Z' -> ReadyForQuery (Bytes.get payload 0)
  | 'E' -> ErrorResponse (parse_error payload)
  | 'T' -> RowDescription (parse_row_description payload)
  | 'D' -> DataRow (parse_data_row payload)
  | 'N' -> NoticeResponse (parse_error payload)
  | 'C' -> CommandComplete (fst @@ get_cstr payload 0)
  | '2' -> BindComplete
  | '1' -> ParseComplete
  | c -> Unknown (c, payload)

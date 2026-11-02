open Parser
open Special
open Ast

module Style = struct
  type t =
    | Plain
    | Command
    | Target
    | Literal
    | Number
    | Operator
    | Punct
    | Invalid
    | Special
    | Column
    | Type
    | Name

  let to_ansi = function
    | Plain -> "\027[0m"
    | Command -> "\027[33m"
    | Target -> "\027[32m"
    | Literal -> "\027[32m"
    | Number -> "\027[1;34m"
    | Operator -> "\027[35m"
    | Punct -> "\027[90m"
    | Invalid -> "\027[4;31m"
    | Special -> "\027[1;36m"
    | Column -> "\027[36m"
    | Type -> "\027[3;94m"
    | Name -> "\027[1;37m"

  let reset = "\027[0m"
end

let style_of_role (r : role) : Style.t =
  match r with
  | Command | Modifier -> Style.Command
  | Target -> Style.Target
  | Name -> Style.Name
  | Column -> Style.Column
  | Operator -> Style.Operator
  | Literal -> Style.Literal
  | Number -> Style.Number
  | Type -> Style.Type
  | Punct -> Style.Punct

let wrap style str = Style.to_ansi style ^ str ^ Style.reset

let add_styled buf style s =
  Buffer.add_string buf (Style.to_ansi style);
  Buffer.add_string buf s;
  Buffer.add_string buf Style.reset

let highlight_query input =
  let n = String.length input in
  match tokenizer input with
  | exception Syntax_error _ -> input
  | toks ->
      let marks : (int, int * role) Hashtbl.t = Hashtbl.create 32 in
      (recorder := fun t r -> Hashtbl.replace marks t.start (t.stop, r));
      let err =
        Fun.protect
          ~finally:(fun () -> recorder := fun _ _ -> ())
          (fun () ->
            match Parser.parse_exn input with
            | exception Syntax_error (Lexer_error { pos; _ }) -> Some pos
            | exception Syntax_error (Parser_error { start; _ }) -> Some start
            | _ -> None)
      in
      let buf = Buffer.create (n * 2) in
      let last = ref 0 in
      let paint style a b =
        Buffer.add_substring buf input !last (a - !last);
        add_styled buf style (String.sub input a (b - a));
        last := b
      in
      let rec paint_token (t : spec) cur =
        let tstop = min t.stop n in
        if cur < tstop then
          match Hashtbl.find_opt marks cur with
          | Some (stop, role) when stop <= tstop ->
              paint (style_of_role role) cur stop;
              paint_token t stop
          | _ ->
              let bad =
                t.tok = Error
                || match err with Some p -> tstop > p | None -> false
              in
              paint (if bad then Style.Invalid else Style.Plain) cur tstop
      in
      List.iter (fun t -> if t.tok <> Eof then paint_token t t.start) toks;
      Buffer.add_substring buf input !last (n - !last);
      Buffer.contents buf

let colorize input =
  match Special.classify input with
  | Special.Special _ -> wrap Style.Special input
  | Special.Unknown name when Special.is_partial name ->
      wrap Style.Special input
  | Special.Unknown _ -> wrap Style.Invalid input
  | Special.Query _ -> highlight_query input

module Style = Highlight.Style

module Table = struct
  type align = Left | Right
  type cell = { text : string; align : align }

  let width s =
    let n = ref 0 in
    String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr n) s;
    !n

  let repeat s n = String.concat "" (List.init n (fun _ -> s))

  let pad cell w =
    let gap = w - width cell.text in
    match cell.align with
    | Left -> cell.text ^ String.make gap ' '
    | Right -> String.make gap ' ' ^ cell.text

  let render ~headers ~rows =
    let widths = Array.of_list (List.map width headers) in
    List.iter
      (List.iteri (fun i c -> widths.(i) <- max widths.(i) (width c.text)))
      rows;

    let paint style str = Highlight.wrap style str in

    let line l m r =
      l
      ^ String.concat m
          (Array.to_list (Array.map (fun w -> repeat "─" (w + 2)) widths))
      ^ r
      |> paint Style.Border
    in

    let row ?(style = Style.Plain) cells =
      let sep = paint Style.Border "│" in
      sep
      ^ String.concat sep
          (List.mapi
             (fun i c ->
               let padded = pad c widths.(i) in
               " "
               ^ (if style = Style.Plain then padded
                  else Highlight.wrap style padded)
               ^ " ")
             cells)
      ^ sep
    in
    let header_row = List.map (fun h -> { text = h; align = Left }) headers in
    String.concat "\n"
      ([
         line "╭" "┬" "╮"; row ~style:Style.Target header_row; line "├" "┼" "┤";
       ]
      @ List.map row rows
      @ [ line "╰" "┴" "╯" ])

  let num s = { text = s; align = Right }
  let txt s = { text = s; align = Left }
end

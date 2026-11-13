let levenshtein a b =
  let n = String.length a and m = String.length b in
  let prev = Array.init (m + 1) Fun.id and cur = Array.make (m + 1) 0 in
  for i = 1 to n do
    cur.(0) <- i;
    for j = 1 to m do
      let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      cur.(j) <- min (min (prev.(j) + 1) (cur.(j - 1) + 1)) (prev.(j - 1) + cost)
    done;
    Array.blit cur 0 prev 0 (m + 1)
  done;
  prev.(m)

let suggest word candidates =
  candidates
  |> List.map (fun c -> (levenshtein word c, c))
  |> List.filter (fun (d, _) -> d <= 2)
  |> List.sort compare
  |> function
  | [] -> None
  | (_, c) :: _ -> Some c

let common_prefix = function
  | [] -> ""
  | first :: rest ->
      List.fold_left
        (fun acc s ->
          let n = min (String.length acc) (String.length s) in
          let i = ref 0 in
          while !i < n && acc.[!i] = s.[!i] do
            incr i
          done;
          String.sub acc 0 !i)
        first rest

let complete prefix candidates =
  candidates |> List.filter (String.starts_with ~prefix) |> function
  | [] -> None
  | [ one ] -> Some one
  | many -> Some (common_prefix many)


let rec take n list =
  match list with
  | [] -> []
  | x :: rest ->
    if x = n then [x]
    else list;;

let rec take n list =
  match list with
  | [] -> []
  | [x] -> [x]
  | head :: rest  ->
    let reversed = List.rev rest in
    let [last :: rest] = reversed in
    List.rev rest
  ;;

let rec take n list =
  match list with
  | [] -> []
  | [x] -> [x]
  | head :: rest  ->
    let a :: b = List.rev list in
    if List.length b = n then List.rev b
    else take n b
  ;;

take 2 [1;2;3];;
take 3 [1;2;3];;

Write a function `split : int list -> int list * int list`

such that
`split l = (front, back)` where `l = back @ List.rev front`

and the length of `back` and `front`
is `List.length l / 2` or `List.length l / 2 + 1`

let split list =
  match list with
  | [] -> []
  | h :: rest -> [h];;

let split list =
  match list with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | [a;b] -> ([a], [b])
  | h :: rest -> ([h], rest);;

let split list =
  match list with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | [a;b] -> ([a], [b])
  | h :: rest ->
    if

let half (front, back) =
  match (front, back) with
  | ([], []) -> ([], [])
  | (f1::f2, b1::b2) -> (f1::f2 @ [b1], b2)
  ;;

let rec half (front, back) =
  match (front, back) with
  | ([], []) -> ([], [])
  | _ ->
    if List.length back <= List.length front then (front, back)
    else
      let b1::b2 = back in
      (front @ [b1], b2)
  ;;


split [1;2;3;4;5];;
half ([1;2;3],[4;5]);;

[], [1;2;3;4;5]
[1], [2;3;4;5]

half ([1], [2;3;4;5]);;

let rec half (front, back) =
  match (front, back) with
  | ([], []) -> ([], [])
  | _ ->
    if List.length back <= List.length front then (front, back)
    else
      let b1::b2 = back in
      (front @ [b1], b2)
  ;;

let split list =
  match list with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | [a;b] -> ([a], [b])
  | h :: rest ->
    half ([h], rest)
    ;;

split [-1; 0; -4; -4; 4; 3; -3]
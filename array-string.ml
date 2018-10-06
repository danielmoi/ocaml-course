(*
  String.compare
  - returns 0 if x = y
  - 1 if x > y
  - -1 if x < y
*)

let is_sorted_pair first second =
  let compared = String.compare first second in
  if compared = 0 then true
  else if compared < 0 then true
  else false

let includes_false arr =
  let result = Array.exists
    (fun x -> x = false)
    arr
  in result;;


let includes_false arr =
  Array.fold_left
  (fun res value ->
    (* exit early, if already true *)
    if res = true then true
    else if value = false then true
    else false
  )
  false
  arr;;

let is_sorted arr =
  let arr_bool = Array.mapi
    (fun index el ->
      print_endline el;
      if index = 0 then true
      else
        let next = arr.(index - 1) in
        is_sorted_pair next el
    )
    arr
    in not (includes_false arr_bool);;


is_sorted [|"bowl"; "diddy"|];;
is_sorted [|"bowl"; "diddy";"elephant"; "elephant" |];;
is_sorted [|"bowl"; "monkey";"diddy";"elephant"; "elephant" |];;
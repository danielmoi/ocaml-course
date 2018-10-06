let min a =
  let result = Array.fold_left
    (fun x y ->
      if (x > y) then y
      else x
    )
    0
    a
  in result
;;

(*
let min a =
  let result = Array.fold_left
    (fun x y ->
      let int_x = float_of_int x in
      let int_y = float_of_int y in
      if (int_x >. int_y) then int_y
      else int_x
    )
    infinity
    a
    in result;;
    *)

let min a =
  let result = Array.fold_left
    (fun x y ->
      if (x > y) then y
      else x
    )
    max_int
    a
  in result
;;

min [| 1 ; 2 ; 3 |];;

(*
Array.iteri doesn't return anything

let get_index arr value =
  let result = Array.iteri
    (fun index el ->
      if (el = value) then index
    )
    arr
  in result
;;
*)

let is_at_index a index value =
  if (a.(index)) = value then true
  else false

let rec get_index a index value =
  if (a.(index) = value) then index
  else get_index a (index + 1) value

let min_index a =
  let min_value = min a in
  let result = get_index a 0 min_value
  in result;;


min_index [| 1; 2; 3 |];;
min_index [| 10; 2; 3 |];;
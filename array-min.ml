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
let rec binary_search arr left right value =
  let mid = left + (right - 1) / 2 in

  if Array.length arr >= 1 then

    (* make sure value exists in array first *)
    if value > arr.(right) then -1
    else if value < arr.(left) then -1


    (* return, we've found it *)
    else if (arr.(mid) = value) then mid

    (* check left sub-array, because value is smaller than middle *)
    else if (arr.(mid) > value) then
      binary_search arr left (mid - 1) value

    (* nup, it's on the right sub-array *)
    else binary_search arr (mid + 1) right value

  else -1;;



let rec binary_search_wrong arr value =
  let half_length = Array.length arr / 2 in


  print_endline ("middle: " ^ (string_of_int half_length));

  if Array.length arr >= 1 then
    (* if value is this half_length, return the index *)
    if arr.(half_length) = value then half_length

    (* if value < half_length value, then its location
      must be in the left sub-array (this is a sorted arr)
    *)
    else if (value < arr.(half_length))
    then
      binary_search_wrong (Array.sub arr 0 half_length) value


    (* the value must be located in the right sub-array *)
    else binary_search_wrong
      (Array.sub arr half_length (Array.length arr - half_length))
      value

  else -1;;


#trace binary_search;;

(* binary_search [|1; 2; 3; 4 |] 2;; *)

let find dict word =
  let right = Array.length dict - 1 in
  let result = binary_search dict 0 right word in
  result;;

find [|"a"; "b"; "c"|] "a";;
find [|"a"; "b"; "c"|] "b";;
find [|"a"; "b"; "c"|] "c";;

find [|"a"; "b"; "c"|] "d";;
find [||] "";;

find [|"b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"|] "a";;

find [|"b"; "c"; "d"|] "l";;
find [|"a"; "b"; "c"; "d"; "e"; "f"; "g"|] "k";;

find [|"d"; "e"; "f"; "g"; "h"; "i"; "j"|] "b";;
find [|"a"; "b"; "c"; "d"; "e"; "f"|] "g";;
(*
*)

#untrace_all;;
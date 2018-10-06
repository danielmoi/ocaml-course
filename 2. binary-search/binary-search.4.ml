let rec binary_search arr left right value =
  if Array.length arr < 1 then -1
  else if (value > arr.(right) || value < arr.(left)) then -1

  else
    let mid = left + (right - 1) / 2 in
    let mid_value = arr.(mid) in
    if (mid_value = value) then mid

    else if (mid_value > value) then
      binary_search arr left (mid - 1) value

    else binary_search arr (mid + 1) right value;;


#trace binary_search;;

(* binary_search [|1; 2; 3; 4 |] 2;; *)

let find dict word =
  let right = Array.length dict - 1 in
  let result = binary_search dict 0 right word in
  result;;

find [|"a"; "b"; "c"|] "b";;
(*
find [|"a"; "b"; "c"|] "a";;
find [|"a"; "b"; "c"|] "c";;

find [|"a"; "b"; "c"|] "d";;
find [||] "";;

find [|"b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"|] "a";;

find [|"b"; "c"; "d"|] "l";;
find [|"a"; "b"; "c"; "d"; "e"; "f"; "g"|] "k";;

find [|"d"; "e"; "f"; "g"; "h"; "i"; "j"|] "b";;
find [|"a"; "b"; "c"; "d"; "e"; "f"|] "g";;
*)

#untrace_all;;
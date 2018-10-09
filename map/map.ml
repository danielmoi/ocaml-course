let map a =
  let result = Array.fold_left
    (fun acc value ->
      Array.append acc [|value|]
    )
    [||]
    a
  in result
;;

let map a f =
  let result = Array.fold_left
    (fun acc value ->
      Array.append acc [|(f value)|]
    )
    [||]
    a
    in result
    ;;

let arr = [|1; 2; 3; |];;

let double a = a * 2;;

let result = map arr double;;

(* val result : int array = [|2; 4; 6|] *)
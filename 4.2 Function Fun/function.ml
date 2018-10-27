let magic a b =
  if a = 1 && b =2 then true
  else false;;
(* val magic : int -> int -> bool = <fun> *)

let magic = fun a b ->
  if a = 1 && b =2 then true
  else false;;

let magic = function a -> function b ->
  if a = 1 && b =2 then true
  else false;;

let magic = function a -> function
| 1,2 -> true
| _ -> false;;
(* val magic : 'a -> int * int -> bool = <fun> *)

let magic = function a -> function b
| 1,2 -> true
| _ -> false;;
(* Error: Variable b must occur on both sides of this | pattern *)

let magic = function a -> function
| (1,2) -> true
| (_,_) -> false;;
(* NUP *)


let magic = function a -> function
| 2 && a=1 -> true
(* syntax error at && *)
| _ -> false;;

(*
  THIS IS IT!!!!!!
  the 2nd pattern match is only for the SECOND function
  *)
let magic = function a -> function
| 2 -> if a=1 then true else false
| _ -> false;;


(* ******************************************************** *)
let rec equal_on_common l1 l2 = match l1,l2 with
  | [],_ -> true
  | _,[] -> true
  | h1::r1,h2::r2 -> h1=h2 && equal_on_common r1 r2;;


let rec equal_on_common = function l1 -> function
| [] -> if l1 = [] then true else false
| h1::r1 ->
  if l1=[] then false
  else let h2::r2 in
    (* Syntax error: pattern expected *)
    h1=h2 && equal_on_common r1 r2
;;

let rec equal_on_common = function l1 -> function
| [] -> if l1 = [] then true else false
| h2::r2 ->
  if l1=[] then true
  else List.hd l1 = h2 && equal_on_common (List.tl l1) r2
;;

(* WORKS, but
You cannot use an auxiliary function List.hd, except &&, = or equaloncommon itself

*)
let rec equal_on_common = function l1 -> function
| [] -> true
| h2::r2 ->
  if l1=[] then true
  else List.hd l1 = h2 && equal_on_common (List.tl l1) r2
;;

(* Not exhaustive *)
let rec equal_on_common = function h1::t1 -> function
| [] -> true
| h2::r2 -> true
;;

let rec equal_on_common = function l1 -> function
| [] -> true
| h2::r2 ->
  if l1=[] then true
  else
    let h1::r1 = l1 in
    h1=h2 && equal_on_common r1 r2)
;;

(* still not exhaustive *)
let rec equal_on_common = function l1 -> function
| [] -> true
| h2::r2 ->
  let h1::r1 = l1 in
  if l1 = [] then true
  else if [h1] = [] then true
  else if r1 = [] then true
  else h1=h2 && equal_on_common r1 r2
;;

let rec equal_on_common = function l1 -> function
| [] -> true
| l2 ->
  if l1 = [] then true
  else if l2 = [] then true
  else
    let h1::r1 = l1 in
    let h2::r2 = l2 in
    true
;;

let rec equal_on_common = function
| [] -> true
(* This should not be a function, it should be a bool *)
| h1::r1 -> function
  | [] -> true
  | h2::r2 -> h1=h2 && equal_on_common r1 r2
;;

let rec equal_on_common = function l1 -> function
| [] -> true
| [h2] ->
  if l1 = [] then true
  else if l1 = [h2] then true
  else false
| h2::r2 ->
  if l1 = [] then true
  else let h1::r1 = l1 in
    h1=h2 && equal_on_common r1 r2
;;



let rec equal_on_common = function l1 -> function
| [] -> true
| [h2] -> let h1::r1 = l1 in
  if l1 = [] then true
  else if r1 != [] then false
  else h1=h2
| h2::r2 -> let h1::r1 = l1 in
  if l1 = [] then true
  else if (r1 = [] && r2 =[]) then true
  else h1=h2 && equal_on_common r1 r2
;;
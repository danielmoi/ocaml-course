(* http://www.mega-nerd.com/erikd/Blog/CodeHacking/Ocaml/fold.html *)

(* FOR .. OR *)
let _ =
    let a = [| 1 ; 2 ; 5 ; 7 ; 11 |] in
    let sum = ref 0 in
    for i = 0 to Array.length a - 1 do
        sum := !sum + a.(i)
    done ;
    Printf.printf "Sum : %d\n" !sum;;


print_endline("----------------------------------------------------");;
(***************************************************************)
(*

The value sum is a reference to an integer which is initialized to zero

The referenced sum is then updated inside the for loop.

Operating on references is a little different to operating on values: it requires
- the use of the de-reference operator "!" to ACCESS the referenced value
- the use of the de-reference assignment operator ":=" to UPDATE the referenced value.
*)

(* Array.iter *)
let _ =
    let a = [| 1 ; 2 ; 5 ; 7 ; 11 |] in
    let sum = ref 0 in
    Array.iter (fun x -> sum := !sum + x) a ;
    Printf.printf "Sum : %d\n" !sum;;


print_endline("----------------------------------------------------");;
(***************************************************************)
(* NB. the "let _ =" is to SCOPE the inner variables; this still works: *)
let a = [| 1 ; 2 ; 5 ; 7 ; 11 |] in
let sum = ref 0 in
Array.iter (fun x -> sum := !sum + x) a ;
Printf.printf "Sum : %d\n" !sum;;


print_endline("----------------------------------------------------");;
(***************************************************************)
let _ =
    let a = [| 1 ; 2 ; 5 ; 7 ; 11 |] in

    (* Let's use fold, as fold left *)
    let fold_left_sum = Array.fold_left (fun x y -> x + y) 0 a in
    Printf.printf "Fold_left sum  : %d\n" fold_left_sum ;

    (* Let's use fold, as fold right *)
    let fold_right_sum = Array.fold_right (fun x y -> x + y) a 0 in
    Printf.printf "Fold_right sum : %d\n" fold_right_sum;;

(*
A single fold_left

Array.fold_left (fun x y -> x + y) 0 a

- 1st parameter is an anonymous function
    - takes 2 params x and y
    - returns their sum

- 2nd parameter is the value passed to the anonymous function,
    the FIRST time it is is called
    - 0
    - for subsequent calls, the value of x will be the return
        value of the previous call of the anonymous function

- 3rd parameter is the array the fold is being applied onto
*)

print_endline("----------------------------------------------------");;
(* We can visualize it better: *)
let _ =
    let a = [| 1 ; 2 ; 5 ; 7 ; 11 |] in
    let fold_left_sum = Array.fold_left
    (   fun x y ->
            Printf.printf "x: %2d y: %2d\n" x y ;
            x + y
            )
        0 a
    (*
          ^
          HERE, the array is passed as 2nd parameter
    *)
    in
    Printf.printf "\nFold_left sum  : %d\n" fold_left_sum;;

    (*
        0    1
        1    2
        3    5
        8    7
        15   11

        Fold_left sum  : 26

    *)

print_endline("----------------------------------------------------");;
(* Let's look at fold_right now
*)
let _ =
    let a = [| 1 ; 2 ; 5 ; 7 ; 11 |] in
    let fold_right_sum = Array.fold_right
    (   fun x y ->
            Printf.printf "x: %2d y: %2d\n" x y ;
            x + y
            )
        a 0
    (*
        ^
        HERE, the array is passed as 2nd parameter
    *)
    in
    Printf.printf "\nFold_right sum : %d\n" fold_right_sum;;

    (*
       11    0
        7   11
        5   18
        2   23
        1   25

        Fold_right sum : 26

    *)

(*
- with fold_left, the function is applied from 1st enty to last entry
- with fold_right, the function is applied in reverse order

- with fold_left, the initializer/previous result is passed as the
    1st parameter to the applied funciton
- with fold_right, the initializer/previous result is passed as the
    2nd parameter

*)
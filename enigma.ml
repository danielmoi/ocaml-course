(* this function only handles integers to 99 *)
let exchange (k: int) =
  let str = string_of_int k in
  prerr_endline str;

  if (String.length str = 1) then k
  else
    let first = String.get str 0 in
    let second = String.get str 1 in
    let reversed = String.make 1 second ^ String.make 1 first in
    int_of_string reversed;;

let is_valid_answer (grand_father_age, grand_son_age) =
  grand_son_age * 4 = grand_father_age &&
  exchange grand_father_age * 3 = exchange grand_son_age
;;
let is_valid_solution solution max_gf min_gs =
  let (age_gf, age_gs) = solution in
  is_valid_answer (age_gf, age_gs) &&
  min_gs <= age_gs &&
  age_gs < age_gf &&
  age_gf <= max_gf;;

let age_gs_from_age_gf age_gf =
  exchange ((exchange age_gf) * 3)
;;

let rec generate_solution age_gf max_gf min_gs =
  let age_gs = age_gs_from_age_gf age_gf in
  let solution = (age_gf, age_gs) in
  if (is_valid_solution solution max_gf min_gs) then solution
  (* not (age_gs < min_gs because age_gs skips everywhere *)
  else if (age_gf > min_gs) then generate_solution (age_gf - 1) max_gf min_gs
  else (-1, -1)
;;


let find input =
  let (max_gf, min_gs) = input in
  generate_solution max_gf max_gf min_gs
;;

(* #trace find;;
#trace generate_solution;;

(* find (82, 21);; *)
find (30, 21);;



#untrace_all;; *)
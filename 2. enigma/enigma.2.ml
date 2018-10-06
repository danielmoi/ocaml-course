let exchange (k: int) =
  let first = String.get (string_of_int k) 0 in
  let second = String.get (string_of_int k) 1 in
  let reversed = String.make 1 second ^ String.make 1 first in
  int_of_string reversed;;

let is_valid_answer (grand_father_age, grand_son_age) =
  grand_son_age * 4 = grand_father_age &&
  exchange grand_father_age * 3 = exchange grand_son_age
;;
let is_valid_solution solution min_gs max_gf =
  let (age_gf, age_gs) = solution in
  min_gs <= age_gs &&
  age_gs < age_gf &&
  age_gf <= max_gf;;

let get_age_gs_from_age_gf age =
  exchange ((exchange age) * 3)
;;
let rec generate_solution age_gf max_gf min_gs =
  let age_gs = get_age_gs_from_age_gf age_gf in
  let solution = (age_gf, age_gs) in

  if age_gs < min_gs then (-1, -1)
  else if (is_valid_solution solution min_gs max_gf) then solution
  else if (not (is_valid_solution solution min_gs max_gf))
  then generate_solution (age_gf - 1) max_gf min_gs
  else (-1, -1)
;;


let find input =
  let (max_gf, min_gs) = input in

  let solution = generate_solution max_gf max_gf min_gs in

  solution
;;
(*

- remove -age- unncessary

- gs_from_gf

- start max_gf
  - get gs
  - exit if < min_gs
  - check if valid
  - if not,
*)
(* #trace generate_solution;;
#trace get_age_gs_from_age_gf;; *)
(* find (72, 18);; *)
find (99, 10);;
find (73, 20);;

(* #untrace generate_solution;;
#untrace get_age_gs_from_age_gf;; *)
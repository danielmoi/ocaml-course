let exchange (k: int) =
  let first = String.get (string_of_int k) 0 in
  let second = String.get (string_of_int k) 1 in
  let reversed = String.make 1 second ^ String.make 1 first in
  int_of_string reversed;;

let is_valid_answer (grand_father_age, grand_son_age) =
  grand_son_age * 4 = grand_father_age &&
  exchange grand_father_age * 3 = exchange grand_son_age
;;

let get_age_gs_from_age_gf age =
  exchange ((exchange age) * 3)
;;
let rec generate_solution age_gf min_age_gs =
  let age_gs = get_age_gs_from_age_gf age_gf in
  let solution = (age_gf, age_gs) in
  if age_gs < min_age_gs then (-1, -1)
  else if (not (is_valid_answer(solution)))
  then generate_solution (age_gf - 1) min_age_gs
  else (-1, -1)
;;
let find input =
  let (max_grand_father_age, min_grand_son_age) = input in

  let solution = generate_solution max_grand_father_age min_grand_son_age in

  solution
;;
(*

- remove -age- unncessary
- start max_gf
*)

find (72, 18);;
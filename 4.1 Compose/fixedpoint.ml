let rec fixedpoint f start delta = 0.0;;

fixedpoint (fun _ -> 10.) -3.18412845028212921 0.0156959751536230215;;

fixedpoint cos 0. 0.001;;

let rec fixedpoint f start delta =
  let result = f start in
  if (result < delta) then result
  else fixedpoint f result delta;;
- this is wrong because we have this type:
val fixedpoint : ('a -> 'a) -> 'a -> 'a -> 'a = <fun>

let rec fixedpoint f start delta =
  let result = f start in
  let diff = start -. result in
  if (diff < delta) then result
  else fixedpoint f result delta;;

Now, we have this:
val fixedpoint : (float -> float) -> float -> float -> float = <fun>



let rec fixedpoint f start delta =
  let result = f start in
  result;;

let rec fixedpoint f start delta =
  let result = f start in
  let diff = result -. start in
  if (diff < delta) then result
  else fixedpoint f result delta;;



let rec fixedpoint f start delta =
  let result = f start in
  let diff = start -. result in
  if (diff < delta) then start
  else fixedpoint f result delta;;


fixedpoint cos 0. 0.001;;
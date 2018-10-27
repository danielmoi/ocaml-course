For instance, compose [f;g;h] x = f (g (h x)).

let rec compose = function
| [a] -> function x -> a x
| [a;b] -> (function x -> a (b x))
;;

let compose f g = function x -> f (g x);;
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>
                              ^ this function has the return type of a function ("'a")

ie. fun -> fun -> x -> y

let rec compose = function
| [f] -> function x -> f x
| [f;g] -> (function x -> g x)
;;

let rec compose = function
| [f] -> fun x -> f x
| [f;g] -> (fun x -> g x)
;;

let rec compose = function
| [f] -> fun x -> f x
| [f;g] -> (fun x -> f (compose g) x)
;;

compose [plus1];;
compose [plus1] 1;;
compose [plus1; plus2];;
compose [plus1; plus2] 1;;

let plus1 = function x -> x + 1;;
let plus2 = function x -> x + 2;;
let plus3 = function x -> x + 3;;


let r a =
    let rec b  =
        match c with
        | [] -> c
        | h :: t -> b (h :: c) t in b [] a

    in b;;

let compose arr =
  let reversed = List.rev arr in
  let apply f x = f x in
  let rec aux l =
    match l with
    | [f] -> function x -> f x
    | [f;g] -> function x -> g (apply f x)
  in aux reversed;;

let compose arr =
  let reversed = List.rev arr in
  let rec aux l =
    match l with
    | [f] -> function x -> f x
    | [f;g] -> function x -> g (f x)
  in aux reversed;;

compose [plus1];;
compose [plus1;plus2];;

# Pattern Matching


# let equal c = match c with
    (x,x) -> true
  | (x,y) -> false;;
Characters 35-36:
This variable is bound several times in this matching

https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora016.html

```ocaml
let match_1 a b =
  match a, b with
  | a, 1 -> 1
  | _ -> 0
  ;;


match_1

let m2 a b =
  match a, b with
  | a, b when a = 1 -> 1
  | _ -> 0
  ;;
m2 1 1;;

Note the differences in List notation

let m3 a b =
  match a, b with
  | [1], 1 -> 1
  | [2; 1], 1 -> 2
  | a::b, 1 -> 3
  | _ -> 0
  ;;

# m3 [] 1;;
- : int = 0
# m3 [1] 1;;
- : int = 1
# m3 [2;1] 1;;
- : int = 2
# m3 [3;1] 1;;
- : int = 3
# m3 [4;1;2] 1;;
- : int = 3
# m3 [4;1;2] 0;;
- : int = 0

The variables in the pattern match (left of the arrow) do NOT refer to the function arguments.
The variables to the right DO
let m4 a b =
  match a, b with
  | x, 1 -> 1
  | x, 2 -> x
  | x, 3 -> a
  | _ -> 0
  ;;

# m4 1 1;;
- : int = 1
# m4 1 2;;
- : int = 1
# m4 1 3;;
- : int = 1

```

------------------------------------------------------------
## Lists: _ only matches ONE element, not the "rest" of the list
```ocaml
let m5 a =
  match a with
  | [x] -> 1
  | [x; _] -> 2
  | [x; _; _] -> 3
  | x::rest -> 4
  | _ -> 5
  ;;

- : int = 1
# m5 [1;2];;
- : int = 2
# m5 [1;2;3];;
- : int = 3
# m5 [1;2;3;4;5];;
- : int = 4
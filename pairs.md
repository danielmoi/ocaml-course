# Pairs

```ocaml
let el = (1,2);;

fst el;;
- : int = 1

```

Or, we can use pattern matching:
```ocaml
let (a,_) = el;;
- val a : int = 1
```
# Tips

1. Get past the first error
```
Found expand with unexpected type:
Wrong type 'b -> string.
```
1. Get the typing correct so we can see the function calls
```ocaml
let factorize exp = exp;;
```
- Found `factorize` with compatible type:

```ocaml
let expand exp =
match exp with
| _ -> exp
;;
```
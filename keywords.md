# Keywords


The `and` keyword is used either to avoid multiple let (first example, I never use it for this but why not) or for mutually recursive definitions of types, functions, modules...

```ocaml
 let rec is_even x =
   if x = 0 then true else is_odd (x - 1)
 and is_odd x =
   if x = 0 then false else is_even (x - 1)
```
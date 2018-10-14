
type trie = Trie of int option * char_to_children
(* ie. a pair, int * list of pairs = int-(char-trie) PAIR *)

and char_to_children = (char * trie) list
(* ie. a list of pairs (char * trie) (char-trie) LIST *)


let empty =
  Trie (None, []);;

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))]);;
```
```ocaml
char_to_children -> char -> trie option

let children_from_char char_trie_list char =
  match char_trie_list char with
  | [(char, list); _] -> Some (char, list)
  | _ -> None
;;

let children_from_char m char =
  match m char with
  | [(char, Trie (x, y)); _] -> Some (Trie (x, y))
  | _ -> None
;;

let children_from_char m char =
(* NOTE THE COMMA HERE *)
  match m, char with
  | [(char, Trie (x, y)); _], _ -> Some (Trie (x, y))
  | _ -> None
;;

let children_from_char m char =
  match m, char with
  | [(char, Trie (x, y))], _ -> Some (Trie (x, y))
  | _ -> None
;;

children_from_char
  [('m', Trie (None, [('j', Trie (Some 3, []))]));
   ('p', Trie (None, [('a', Trie (Some 3, []))]));
   ('j', Trie (Some 2, [('m', Trie (None, [('g', Trie (Some 0, []))]))]))]
  'p';;

children_from_char
  [('p', Trie (None, []))]
  'p';;

let match_first m char =
  match m, char with
  | [(char, _)], _ -> 1
  | _ -> 0
  ;;


let match_first m char =
  match m, char with
  | [(char, Trie(x, y))], _ -> 1
  | _ -> 0
  ;;

Doesn't match, with the ;_
let match_first m char =
  match m, char with
  | [(char, Trie(x, y)); _], _ -> 1
  | _ -> 0
  ;;


match_first
  [('p', Trie (None, []))]
  'p';;


Now with a list > 1

let rec match_first m char =
  match m, char with
  | [(char, Trie(x, y))], _ -> Some (Trie (x, y))
  | h::rest, _ -> match_first rest char
  | _ -> None
  ;;


match_first
  [
    ('q', Trie (None, []));
    ('p', Trie (None, []));
  ]
  'p';;

let rec children_from_char m char =
  match m, char with
  | [(char, Trie(x, y))], _ -> Some (Trie (x, y))
  | [(char, Trie(x, y)); _], _ -> Some (Trie (x, y))
  | h::rest, _ -> children_from_char rest char
  | _ -> None
  ;;


match_first
    [('m', Trie (None, [('j', Trie (Some 3, []))]));
   ('p', Trie (None, [('a', Trie (Some 3, []))]));
   ('j', Trie (Some 2, [('m', Trie (None, [('g', Trie (Some 0, []))]))]))]
  'p';;

children_from_char
  [('a', Trie (None, [('d', Trie (Some (-2), []))]));
   ('s', Trie (None, [('d', Trie (Some (-2), []))]));
   ('g', Trie (None, [('d', Trie (Some 2, []))]));
   ('j', Trie (None, [('s', Trie (None, [('p', Trie (Some (-1), []))]))]));
   ('p', Trie (None, [('p', Trie (Some (-4), []))]));
   ('m', Trie (None, [('j', Trie (None, [('d', Trie (Some (-1), []))]))]))]
  'g';;

let rec children_from_char m char =
  match m, char with
  (* | [(char, Trie(x, y))], _ -> Some (Trie (x, y)) *)
  | [(char, Trie(x, y)); _], _ -> Some (Trie (x, y))
  (* | h::rest, _ -> children_from_char rest char *)
  | _ -> None
  ;;

children_from_char
  [
   ('g', Trie (None, [('d', Trie (Some 2, []))]));
   ('j', Trie (None, [('s', Trie (None, [('p', Trie (Some (-1), []))]))]));
   ('p', Trie (None, [('p', Trie (Some (-4), []))]));
   ('m', Trie (None, [('j', Trie (None, [('d', Trie (Some (-1), []))]))]))]
  'g';;

let rec children_from_char m char =
  match m, char with
  (* | [(char, Trie(x, y))], _ -> Some (Trie (x, y)) *)
  | [(char, Trie(x, y)); _], _ -> Some (Trie (x, y))
  (* | h::rest, _ -> children_from_char rest char *)
  | _ -> None
  ;;

let rec children_from_char m char =
  match m, char with
  | [(char, y)], _ -> Some y
  | _ -> None
  ;;

children_from_char
  [
   ('g', Trie (None, [('d', Trie (Some 2, []))]));
  ]
  'g';;

let rec children_from_char m char =
  match m, char with
  | [(char, Trie (x,y))], _ -> Some (Trie (x,y))
  | head::rest, _ -> children_from_char rest char
  | _ -> None
  ;;
this matches
children_from_char
  [
   ('p', Trie (None, [('p', Trie (Some (-4), []))]));
   ('g', Trie (None, [('d', Trie (Some 2, []))]));
  ]
  'g';;

let rec children_from_char m char =
  print_char char;
  let h::t = m in
  print_int (List.length t);
  match m, char with
  (* | [(char, Trie (x,y))], _ -> Some (Trie (x,y)) *)
  | [(a, Trie (x,y))], b when a = char -> Some (Trie (x,y))
  | [(a, Trie (x,y)); _], b when a = char -> Some (Trie (x,y))
  | head::rest, _ -> children_from_char rest char
  | _ -> None
  ;;
children_from_char
  [
   ('p', Trie (None, [('p', Trie (Some (-4), []))]));
   ('g', Trie (None, [('d', Trie (Some 2, []))]));
   ('s', Trie (None, [('d', Trie (Some (-2), []))]));
  ]
  'g';;

let rec children_from_char m char =
  match m, char with
  (* | [(char, Trie (x,y))], _ -> Some (Trie (x,y)) *)
  | [(a, Trie (x,y))], b when a = char ->
    print_string "ONE";
    Some (Trie (x,y))
  (* | [(a, Trie (x,y)); _], b when a = char ->  *)
  | (a, Trie(x,y))::rest, _ when a = char ->
    print_string "TWO";
    Some (Trie (x,y))

  | (a, Trie(x,y))::rest, _ ->
    print_string "THREE";
    children_from_char rest char
  | _ -> None
  ;;

let rec children_from_char m char =
  match m, char with
  | [(a, Trie (x,y))], b when a = char -> Some (Trie (x,y))
  | (a, Trie(x,y))::rest, _ when a = char -> Some (Trie (x,y))
  | (a, Trie(x,y))::rest, _ -> children_from_char rest char
  | _ -> None
  ;;

children_from_char
  [
   ('a', Trie (None, [('d', Trie (Some (-2), []))]));
   ('s', Trie (None, [('d', Trie (Some (-2), []))]));
   ('g', Trie (None, [('d', Trie (Some 2, []))]));
   ('j', Trie (None, [('s', Trie (None, [('p', Trie (Some (-1), []))]))]));
   ('p', Trie (None, [('p', Trie (Some (-4), []))]));
   ('m', Trie (None, [('j', Trie (None, [('d', Trie (Some (-1), []))]))]))
   ]
  'g';;
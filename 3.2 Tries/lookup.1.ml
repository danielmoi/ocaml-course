type trie = Trie of int option * char_to_children
(* ie. a pair, int * list of pairs = int-(char-trie) PAIR *)

and char_to_children = (char * trie) list
(* ie. a list of pairs (char * trie) (char-trie) LIST *)

let rec children_from_char m char =
  match m, char with
  | [(a, Trie (x,y))], b when a = char -> Some (Trie (x,y))
  | (a, Trie(x,y))::rest, _ when a = char -> Some (Trie (x,y))
  | (a, Trie(x,y))::rest, _ -> children_from_char rest char
  | _ -> None
  ;;

let explode s = List.init (String.length s) (String.get s);;

let get_cc_from_trie trie =
  match trie with
  | Trie (oi, cc) -> cc;;


let lookup trie str =
  let chars = explode str in
  NUP:
  let (i, list) = trie in
  let result = List.fold_left
  (fun acc c ->
    if (children_from_char list c = None) then Some 10
    else Some 1
  )
  None
  chars
  in
  result
;;

let lookup trie str =
  let chars = explode str in
  let list = get_cc_from_trie trie in
  let result = List.fold_left
  (fun acc c ->
    if (children_from_char list c = None) then Some 10
    else Some 1
  )
  None
  chars
  in
  result
;;

let lookup_char trie char =
  let cc = get_cc_from_trie in
  let result = children_from_char cc char in
  result
;;


lookup
  (Trie (Some 1,
    [('j', Trie (None, [('s', Trie (Some 4, []))]));
     ('s', Trie (None, [('m', Trie (None, [('j', Trie (Some (-5), []))]))]));
     ('m', Trie (None, [('a', Trie (Some (-1), []))]));
     ('d', Trie (None, [('a', Trie (None, [('g', Trie (Some (-5), []))]))]))]))
  "dag";;


let trie1 = (Trie (Some 1,
  [('j', Trie (None, [('s', Trie (Some 4, []))]));
    ('s', Trie (None, [('m', Trie (None, [('j', Trie (Some (-5), []))]))]));
    ('m', Trie (None, [('a', Trie (Some (-1), []))]));
    ('d', Trie (None, [('a', Trie (None, [('g', Trie (Some (-5), []))]))]))]));;
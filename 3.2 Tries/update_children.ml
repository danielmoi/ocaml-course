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

let update_children list char t =
  if children_from_char list char = None then list
  else list
;;

let update_children list char t =
  if children_from_char list char = None then
    let el = (char, t) in
    el::list
  else list
;;

update_children
  [('g', Trie (None, [('m', Trie (Some 3, []))]));
   ('p', Trie (Some 1, [('g', Trie (None, [('a', Trie (Some 3, []))]))]))]
  'j'
  (Trie (Some 1,
    [('m', Trie (None, [('s', Trie (Some (-5), []))]));
     ('d', Trie (None, [('j', Trie (None, [('a', Trie (Some (-5), []))]))]));
     ('p', Trie (None, [('g', Trie (None, [('m', Trie (Some 2, []))]))]));
     ('j', Trie (None, [('j', Trie (Some (-5), []))]));
     ('a', Trie (Some 0, [('j', Trie (None, [('g', Trie (Some 1, []))]))]))]));;

let update_children list char t =
  if children_from_char list char = None then
    let el = (char, t) in
    el::list
  else List.map
    (fun el ->
      if fst el = char then (char, t)
      else el
    )
    list
;;

update_children
  [('p', Trie (None, [('g', Trie (Some (-5), []))]));
   ('g', Trie (None, [('p', Trie (Some (-4), []))]));
   ('m',
    Trie (Some 0,
     [('d', Trie (None, [('g', Trie (Some 2, []))]));
      ('p', Trie (None, [('j', Trie (Some 0, []))]));
      ('g', Trie (None, [('s', Trie (Some (-4), []))]))]))]
  'p'
  (Trie (Some 1,
    [('d', Trie (None, [('j', Trie (Some (-1), []))]));
     ('a',
      Trie (None,
       [('s', Trie (Some (-1), []));
        ('m', Trie (None, [('p', Trie (Some (-1), []))]))]))]));;

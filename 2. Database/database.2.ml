(* A phone number is a sequence of four integers. *)
type phone_number = int * int * int * int;;

(* A contact has a name and a phone number. *)
type contact = {
  name         : string;
  phone_number : phone_number
};;

(* Here is a dumb contact. *)
let nobody = { name = ""; phone_number = (0, 0, 0, 0) };;

(* A database is a collection of contacts. *)
type database = {
  number_of_contacts : int;
  contacts : contact array;
};;

(* [make n] is the database with no contact and at most [n] contacts
    stored inside. *)
let make max_number_of_contacts =
  {
    number_of_contacts = 0;
    contacts = Array.make max_number_of_contacts nobody
  };;

(* Queries are represented by a code and a contact.
   - If the code is 0 then the contact must be inserted.
   - If the code is 1 then the contact must be deleted.
   - If the code is 2 then we are looking for a contact
     with the same name in the database. *)
type query = {
  code    : int;
  contact : contact;
}

let search db contact =
  let rec aux idx =
    if idx >= db.number_of_contacts then
      (false, db, nobody)
    else if db.contacts.(idx).name = contact.name then
      (true, db, db.contacts.(idx))
    else
      aux (idx + 1)
  in
  aux 0;;

let insert db contact =
  if db.number_of_contacts >= Array.length db.contacts then
    (false, db, nobody)
  else
    let (status, db, _) = search db contact in
    if status then (false, db, contact) else
      let cells i =
	if i = db.number_of_contacts then contact else db.contacts.(i)
      in
      let db' = {
          number_of_contacts = db.number_of_contacts + 1;
          contacts = Array.init (Array.length db.contacts) cells
        }
      in
      (true, db', contact);;

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let cells i =
      if db.contacts.(i).name = contact.name then
        nobody
      else
        db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts - 1;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in
    (true, db', contact);;

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let cells =
      Array.fold_left
      (fun contacts el ->
        if el.name = contact.name
          then Array.append contacts [nobody]
          (* then contacts *)
        else Array.append contacts [|el|]
      )
      (* not:
      [|db.contacts.(0)|]
      *)
      [||]
      db.contacts
      in
    let db' = {
        number_of_contacts = db.number_of_contacts - 1;
        contacts = cells
      }
    in
    (true, db', contact);;

(* Engine parses and interprets the query. *)
let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else (false, db, nobody);;

(*
let db = make 5;;
  let contact1 = {
    name = "Pikachu";
    phone_number = (1,2,3,4);
  } in
  let contact2 = {
    name = "Mewtwo";
    phone_number = (1,2,3,4);
  } in
  let insert1 = insert db contact1 in
  insert1;;
  let search1 = search db contact1 in
  (* let insert2 = insert db contact2 in
  let search2 = search db contact2 in
  search2;; *)

(* incorrect syntax
  let proof_of_bug =
  [|
    "let db = make 5";
    "let contact1 = { name = "Pikachu"; phone_number = (1, 2, 3, 4) }";
  |];;
*)

*)

let proof_of_bug =
  let contact1 = { name = "Pikachu"; phone_number = ( 1, 2, 3, 4) } in
  let db = make 0 in
  [|
    engine db { code = 0; contact = contact 1 };
  |];;

let proof_of_bug =
  let contact1 = { name = "Pikachu"; phone_number = (1, 2, 3, 4) } in
  let contact2 = { name = "Pichu"; phone_number = (1, 2, 3, 4) } in
  let db = make 0 in
  [|
    { code = 0; contact = contact1 };
    { code = 0; contact = contact2 };
    { code = 1; contact = contact1 };
    { code = 2; contact = contact2 };
  |];;
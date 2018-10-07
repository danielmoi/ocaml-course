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
          then
            Array.append contacts [|nobody|]
          (* then contacts *)

          (* this adds an empty to the end *)
        (* else if el.name = "" then Array.append contacts [|nobody|] *)

        (* this just returns the contact list *)
        (* else if el.name = "" then contacts *)

        else if el.name = "" then Array.append contacts [|nobody|]

        (* else Array.append contacts [|el|] *)
        (* move all contacts to the top, keeping the same number of ALL contacts, including empties *)
        else Array.append [|el|] contacts
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

let update db contact =
  (* NB. this was hiding the "contact" below *)
  let (status, db, existing) = search db contact in

  if not status then insert db contact
  else
    let cells =
      Array.fold_left
      (fun contacts el ->
        if el.name = contact.name
          then
            (* prerr_endline "HELLO" in *)
            Array.append contacts [| contact |]
        else if el.name = ""
          then Array.append contacts [|nobody|]
        else Array.append [|el|] contacts
      )
      [||]
      db.contacts
      in
      let db' = {
        number_of_contacts = db.number_of_contacts;
        contacts = cells;
      }
      in
      (true, db', contact);;


(* Engine parses and interprets the query. *)
let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;

let db = make 5 in
  let x = engine db { code = 0; contact = { name = "A"; phone_number = (1,1,1,1) } } in
  let (_, new_db, _) = x in
  let y = engine new_db { code = 0; contact = { name = "B"; phone_number = (1,1,1,1) } } in
  let (_, new_db, _) = y in
  let z = engine new_db { code = 1; contact = { name = "A"; phone_number = (1,1,1,1) } } in
  let (_, new_db, _) = z in
  let a = engine new_db { code = 2; contact = { name = "B"; phone_number = (1,1,1,1) } } in
  let (_, new_db, _) = a in
  let b = engine new_db { code = 3; contact = { name = "B"; phone_number = (2,2,2,2) } } in
  (x, y, z, a, b)
;;

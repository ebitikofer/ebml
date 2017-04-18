let rec remove_last lst =
  match lst with
    [] -> []
    | hd::tl when tl <> [] -> hd::remove_last tl
    | _ -> []
;;

let rec unique_el el lst =
  match lst with
    [] -> []
    | hd::tl when hd <> el -> hd::unique_el el tl
    | hd::tl when hd = el -> el::(remove_el el tl)
    | hd::_ when hd = el -> []
and remove_el el lst =
  match lst with
    [] -> []
    | hd::tl when hd <> el -> hd::remove_el el tl
    | hd::_ when hd = el -> remove_el el tl
;;

(*
let rec find_multiples lst =
  match lst with
    [] -> []
    | first::second::tl -> unique_el first second::tl find_multiples(x.tail)
    | hd::_ -> hd::[]

and unique el lst =
  match lst with
    [] -> []
    | hd::tl when hd <> el -> hd::unique el tl
    | hd::tl when hd = el -> el::remove_l tl
    | hd::_ when hd <> el -> hd::[]
    | hd::_ when hd = el -> []
and remove_l el lst =
  match lst with
    [] -> []
    | hd::tl when hd <> el -> hd::remove_l el tl
    | hd::tl when hd = el -> remove el tl
    | hd::_ when hd <> el -> hd::[]
    | hd::_ when hd = el -> []
;;
*)
(*
let rec keep_two lst =
  match lst with
    [] -> []
    | if item is not in single add to single and add to final
    | if item is not in double add to double and add to final
    | if item is in double continue
*)

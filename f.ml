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
    | hd::tl when hd = el -> el::remove_el el tl
and remove_el el lst =
  match lst with
    [] -> []
    | hd::tl when hd <> el -> hd::remove_el el tl
    | hd::tl when hd = el -> remove_el el tl
;;

let rec find_multiples lst =
  match lst with
    [] -> []
    | hd::tl when member hd tl = false -> find_multiples (remove_el hd tl)
    | hd::tl when member hd tl = true -> hd::find_multiples (remove_el hd tl)
and member el lst =
  match lst with
    [] -> false
    | hd::tl when hd <> el -> member el tl
    | hd::tl when hd = el -> true
;;

let rec keep_two lst =
  match lst with
    [] -> []
    | hd::tl -> hd::unique_el hd (keep_two tl)
;;

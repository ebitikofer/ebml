(*2016.eric.bitikofer*)

(*remove_last - takes in a list and removes the last element in the list*)
let rec remove_last lst =
  match lst with
    [] -> []
    | hd::tl when tl <> [] -> hd::remove_last tl
    | _ -> []
;;

(*unique_el - takes in an element and a list and removes all duplicates of that element in the list*)
(*uses remove_el as a helper*)
let rec unique_el el lst =
  match lst with
    [] -> []
    | hd::tl when hd <> el -> hd::unique_el el tl
    | hd::tl when hd = el -> el::remove_el el tl
and remove_el el lst = (*remove_el - takes in an element and a list and removes all of that element from the list*)
  match lst with
    [] -> []
    | hd::tl when hd <> el -> hd::remove_el el tl
    | hd::tl when hd = el -> remove_el el tl
;;

(*find_multiples - takes in a list and produces a list from the repeated elements in that list*)
(*uses member as a helper*)
let rec find_multiples lst =
  match lst with
    [] -> []
    | hd::tl when member hd tl = false -> find_multiples (remove_el hd tl)
    | hd::tl when member hd tl = true -> hd::find_multiples (remove_el hd tl)
and member el lst = (*member - takes in an element and a list and checks to see if its a member*)
  match lst with
    [] -> false
    | hd::tl when hd <> el -> member el tl
    | hd::tl when hd = el -> true
;;

(*keep_two - takes in a list and keeps the first two copies of each element in the list*)
let rec keep_two lst =
  match lst with
    [] -> []
    | hd::tl -> hd::unique_el hd (keep_two tl)
;;

(*originated as hw6 in CS3200*)

open Stdio

let rec list_length l =
  match l with
  | [] -> 0
  | _ :: tl -> 1 + list_length tl

  let() =
  printf "list length is: %i\n"
   (list_length ["OCaml";"Perl";"C";"test";"test"])

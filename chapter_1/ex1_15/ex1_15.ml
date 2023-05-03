open Base
open Stdio
(* open Base.Poly *)




let dupl n s = 
   let rec aux n s acc =
   if n=0 then acc
   else aux (n-1) s (s::acc) in
   aux n s []

let rec print_list = function
| [] -> printf ""
| head::tail -> printf "%c " head; print_list tail
 


let () =
print_list (dupl 0 'a');
printf "\n"


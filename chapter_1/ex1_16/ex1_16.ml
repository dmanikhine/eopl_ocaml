open Base 
open Stdio
(* open Base.Poly *)


let v=[['a';'b'];['a';'b']]


let rec print_list = function
| [] -> printf ""
| head1::tail1 ->
    match head1 with
    | [] -> printf ""
    | head2::tail2 ->
      match tail2 with
      | [] -> printf ""
      | head3::_ -> printf "['%c';" head2; printf "'%c']" head3; print_list tail1

let rec print_list_v2 = function
| [] -> printf ""
| (f::s::_)::tail1 -> printf "['%c';" f; printf "'%c']" s; print_list_v2 tail1
| _::_ -> printf ""


   let invert lst = 
   let rec aux acc = function
   | [] -> acc
   | head1::tail1 ->
       match head1 with
       | [] -> aux acc tail1
       | head2::tail2 ->
         match tail2 with
         | [] -> aux acc tail1
         | head3::_ -> aux ((head3::head2::[])::acc) tail1 in 
   aux [[]] lst

   let invert_v2 lst = 
      let rec aux acc = function
      | [] -> acc
      | (f::s::tail2)::tail -> aux ((s::f::tail2)::acc) tail
      | _::_ -> acc in
       aux [[]] lst



let () = printf "\n";
print_list_v2 (invert_v2 v);
printf "\n\n";


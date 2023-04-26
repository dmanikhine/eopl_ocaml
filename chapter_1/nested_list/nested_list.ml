open Base
open Stdio 
(* open Base.Poly *)




type 'a lcExp =
  | One of 'a
  | Many of 'a lcExp list

let flatten l =
    let rec aux acc = function
      | [] -> acc
      | One head :: tail ->  aux (head :: acc) tail
      | Many l :: tail -> aux (aux acc l) tail in
    aux [] l

let rec aux  = function
        | [] -> printf " \n"
        | head :: tail -> printf "%s" head; aux tail                      

let () = aux (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ])
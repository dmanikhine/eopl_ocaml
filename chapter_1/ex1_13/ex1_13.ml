open Base
open Stdio
(* open Base.Poly *)


type nListChar = 
|O of char
|M of nListChar list

let v=[O('a');O('b');O('c');M([O('a');O('b');O('c')]);]

let rec subst sold snew lonListChar=
List.map ~f:(subst_in_s_exp sold snew) lonListChar
and
subst_in_s_exp sold snew = function
|O(x) ->
   if (Char.equal x sold) then
   O(snew)
   else O(x)
|M(x) ->
   M(subst sold snew x)
 



let rec print_nListChar = function
|[] -> printf ""
|O(x)::tail -> printf "O(%c);" x; print_nListChar tail
|M(los)::tail -> printf "M([:"; print_nListChar los; printf "]);"; print_nListChar tail


let () =
print_nListChar v; printf " \n";
print_nListChar (subst 'a' 'z' v); printf " \n";
printf "\n";
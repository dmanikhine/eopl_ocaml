open Base
open Stdio
(*open Base.Poly *)


type nListChar = 
|O of char
|M of nListChar list

let v=[O('a');O('b');O('c');M([O('a');O('b');O('c')]);]

let rec subst sold snew = function
| [] -> []
| O(x):: tail ->
  if (Char.equal x sold) then
    O(snew)::(subst sold snew tail)
    else O(x)::(subst sold snew tail)
| M(los)::tail -> M(subst sold snew los)::(subst sold snew tail)



let rec subst_v2 sold snew = function
| [] -> []
| head::tail -> (subst_in_s_exp sold snew (head))::subst_v2 sold snew tail
and
(*subst_in_s_exp sold snew= function
| O(x) when (Char.equal x sold) -> printf "change for %c \n" snew; O(snew)
| O(any) -> printf "not change %c \n" any; O(any)
| M(x) -> printf "change M sold:%c snew:%c \n" sold snew; M(subst_v2 sold snew x)*)
subst_in_s_exp sold snew = function
|O(x) ->
   if (Char.equal x sold) then
   O(snew)
   else O(x)
|M(x) ->
   M(subst_v2 sold snew x)


 



let rec print_nListChar = function
|[] -> printf ""
|O(x)::tail -> printf "O(%c);" x; print_nListChar tail
|M(los)::tail -> printf "M([:"; print_nListChar los; printf "]);"; print_nListChar tail


let () =
print_nListChar v; printf " \n";
print_nListChar (subst 'a' 'z' v); printf " \n";
printf "\n";
print_nListChar v; printf " \n";
print_nListChar (subst_v2 'a' 'z' v); printf " \n";


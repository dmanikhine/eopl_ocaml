open Base
open Stdio
(* open Base.Poly *)

let base = 100
let base_sub_1 = base-1
let zero =[]

let is_zero = function
| [] -> true
| _::_ -> false


let rec successor = function
| [] -> [1]
| x::tail when x=base_sub_1 -> 0::(successor tail)
| x::tail -> (x+1)::tail

let rec predecessor = function
| [] -> zero
| [1] -> zero
| x::tail when x=0 -> base_sub_1::(predecessor tail)
| x::tail -> x-1::tail

let rec plus x y =
  if (is_zero x) then y
  else plus (predecessor x) (successor y)

let  multiply m n =
  let rec aux acc m n =
    if (is_zero n) then acc
    else aux (plus acc m) m (predecessor n) in 
  aux zero m n 
  
let factorial n =
  let rec aux acc n =
    if (is_zero n) then acc
    else aux (multiply acc n) (predecessor n) in 
  aux (successor zero) n 



let () =
printf "\n";
List.iter ~f:(printf "%i ") (factorial [14]);
printf "\n\n"



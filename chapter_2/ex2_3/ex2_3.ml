open Base
open Stdio
open Base.Poly

type diff_tree =
| One 
| Diff of diff_tree*diff_tree

let zero= Diff(One,One)

let rec diff_tree_to_integer = function
| One -> 1
| Diff(x,y) -> (diff_tree_to_integer x) - (diff_tree_to_integer y)

let is_zero n =
  if (diff_tree_to_integer n)=0 then true
  else false

let successor n =
  Diff(n,Diff(zero,One))

let predcessor n =
  Diff(n,One)  

let diff_tree_plus x y =
  match y with
  | One -> predcessor x
  | Diff(c,d) -> Diff(x,Diff(d,c))

 let rec integer_to_diff_tree = function
 | 1 -> One 
 | x when x>0 -> successor (integer_to_diff_tree (x-1))
 | x -> predcessor (integer_to_diff_tree (x+1))


let () =
let diff_result = diff_tree_plus (integer_to_diff_tree 25) (integer_to_diff_tree 25) in
let int_result=diff_tree_to_integer(diff_result) in
printf "\n%i\n\n" int_result






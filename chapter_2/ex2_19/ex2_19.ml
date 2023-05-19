open Base 
open Stdio
(* open Base.Poly *)

type bintree=
| Leaf
| Node of {top: int; l_t: bintree; r_t:bintree}

let number_to_bintree num = 
   Node {top=num;l_t=Leaf; r_t=Leaf;}

let insert_to_left num bt=
match bt with
| Leaf -> number_to_bintree num
| Node {top=n;l_t=l; r_t=r;} -> Node {
                                top=n;
                                l_t=Node {top=num;l_t=l;r_t=Leaf;};
                                r_t=r} 

let insert_to_right num bt=
match bt with
| Leaf -> number_to_bintree num
| Node {top=n;l_t=l; r_t=r;} -> Node {
                                top=n;
                                l_t=l;
                                r_t=Node {top=num;l_t=r;r_t=Leaf;};} 

let move_to_left = function
|Leaf ->Leaf
|Node x -> x.l_t

let move_to_right = function
|Leaf ->Leaf
|Node x -> x.r_t

let is_at_leaf = function
|Leaf -> true
|_ -> false

let current_element = function
|Node x -> x.top
|_ -> failwith "no element in Leaf"

let t1=(insert_to_right 14 (insert_to_left 12 (number_to_bintree 13)))
let t2=move_to_left t1
let t3=move_to_left (insert_to_left 15 t1)


let () = printf "\n current element in t1 is %i \n\n" (current_element t1);
printf "\n current element in t2 is %i \n\n" (current_element t2);
printf "\n at Leaf is %b \n\n" (is_at_leaf (move_to_right (move_to_left t1)));
printf "\n current element in t3 is %i \n\n" (current_element t3);

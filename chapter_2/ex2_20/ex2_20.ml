open Base 
open Stdio
(* open Base.Poly *)

type bintree=
| Leaf
| Node of {top: int; l_t: bintree; r_t:bintree}

type parents=
|PLeft of int*bintree
|PRight of int*bintree

type bintree_parents={b:bintree;p:parents list}

let number_to_bintree num = 
   {b=Node {top=num;l_t=Leaf; r_t=Leaf;};p=[]}

let insert_to_left num bp=
match bp.b with
| Leaf -> number_to_bintree num
| Node {top=n;l_t=l; r_t=r;} -> {b=Node {top=n; l_t=Node {top=num;l_t=l;r_t=Leaf;}; r_t=r};
                                 p=bp.p} 

let insert_to_right num bp=
match bp.b with
| Leaf -> number_to_bintree num
| Node {top=n;l_t=l; r_t=r;} -> {b=Node {top=n; l_t=l; r_t=Node {top=num;l_t=r;r_t=Leaf;};};
                                 p=bp.p}

let move_to_left bp=
match bp.b with
|Leaf ->{b=Leaf;p=bp.p}
|Node x -> {b=x.l_t;p=PRight(x.top,x.r_t)::bp.p}

let move_to_right bp= 
match bp.b with
|Leaf ->{b=Leaf;p=bp.p}
|Node x -> {b=x.r_t;p=PLeft(x.top,x.l_t)::bp.p}

let is_at_root bp=
match bp.p with
| [] -> true
| _ -> false

let first_and_tail = function
| [] -> failwith "Function first_el: list is empty"
| x::y -> (x,y)

let move_up bp=
if (is_at_root bp) then bp
else let (f,t)=first_and_tail bp.p in
   match f with
   | PLeft(num,l) -> {b=Node {top=num;l_t=l;r_t=bp.b};p=t}
   | PRight(num,r)-> {b=Node {top=num;l_t=bp.b;r_t=r};p=t}




let is_at_leaf bp = 
match bp.b with
|Leaf -> true
|_ -> false

let current_element bp = 
match bp.b with   
|Node x -> x.top
|_ -> failwith "no element in Leaf"

let t1=(insert_to_right 14 (insert_to_left 12 (number_to_bintree 13)))
let t2=move_to_left t1
let t3=move_to_left (insert_to_left 15 t1)
let t4=move_up t3


let () = printf "\n current element in t1 is %i \n\n" (current_element t1);
printf "\n current element in t2 is %i \n\n" (current_element t2);
printf "\n at Leaf is %b \n\n" (is_at_leaf (move_to_right (move_to_left t1)));
printf "\n current element in t3 is %i \n\n" (current_element t3);
printf "\n current element in t4 is %i \n\n" (current_element t4);
printf "\n t4 is at root: %b \n\n" (is_at_root t4);
printf "\n t3 is at root: %b \n\n" (is_at_root t3)
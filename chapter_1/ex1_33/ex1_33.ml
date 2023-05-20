open Base 
open Stdio
open Base.Poly

type bintree=
| Ln of int (*Leaf node *) 
| In of {key:string;l_t:bintree;r_t:bintree} (*Interior node *)

let bt_l1= In {key="bar";l_t=Ln 26;r_t=Ln 12}
let bt_r0=In {key="quux";l_t=Ln 117;r_t=Ln 14}
let bt_r1=In {key="red";l_t=Ln 11; r_t=bt_r0}
let bt=In {key="red";l_t=bt_l1; r_t=bt_r1}

let contents_of_in = function
| Ln _ -> failwith "Errror! Leaf ist not node!!!"
| In {key=x;l_t=_;r_t=_} -> x

let contents_of_ln = function
| In _ -> failwith "Errror! Leaf ist not node!!!"
| Ln x -> x

let move_to_left = function
| Ln x -> Ln x
| In {key=_;l_t=l;r_t=_} -> l

let move_to_right = function
| Ln x -> Ln x
| In {key=_;l_t=_;r_t=r} -> r

let rec bt_double = function
|Ln x -> Ln (x+x)
|In {key=k;l_t=l;r_t=r} -> In{key=k;l_t=bt_double l; r_t=bt_double r}

let bt2=bt_double bt

let mark_leaves_with_red_depth bt =
let rec aux acc = function
|Ln _ -> Ln acc
|In {key=k;l_t=l;r_t=r} -> if (k="red") then In{key=k;l_t=aux (acc+1) l; r_t=aux (acc+1) r}  
else In{key=k;l_t=aux acc l; r_t=aux acc r} in 
aux 0 bt  

let leaf1 = (move_to_left (move_to_left bt2))
let bt_red=mark_leaves_with_red_depth bt

let red_left_leaf=(move_to_left (move_to_left bt_red))
let red_right_leaf=move_to_right (move_to_right (move_to_right bt_red))


let ()=
printf "\n \n contents of bt is %s \n\n" (contents_of_in bt);
printf "\n \n contents of leaf1 is %i \n\n" (contents_of_ln leaf1);
printf "\n \n contents of read_left_leaf is %i \n\n" (contents_of_ln red_left_leaf);
printf "\n \n contents of read_right_leaf is %i \n\n" (contents_of_ln red_right_leaf)
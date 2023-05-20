open Base 
open Stdio
(*open Base.Poly*)

type bintree=
| Ln of int (*Leaf node *)
| In of {key:string;l_t:bintree;r_t:bintree} (*Interior node *)


type taux={nsum:int;max_sum:int;max_node:string}

let tree1 =In {key="foo";l_t=Ln 2; r_t=Ln 3 }
let tree2 =In {key="bar";l_t=Ln(-1); r_t=tree1 }
let tree3 =In {key="baz";l_t=tree2; r_t=Ln 1 } 

let max_leaf_interior key leaf interior =
let node_sum=interior.max_sum+leaf in
if (interior.max_sum > node_sum) then {nsum=node_sum;max_sum=interior.max_sum;max_node=interior.max_node}
else {nsum=node_sum;max_sum=node_sum;max_node=key}


let max_interior_interior key in1 in2 =
  let node_sum=in1.max_sum+in2.max_sum in
  if ((node_sum>in1.max_sum) && (node_sum>in2.max_sum)) then {nsum=node_sum;max_sum=node_sum;max_node=key}
  else if ((node_sum<in1.max_sum) && (in1.max_sum>in2.max_sum)) then {nsum=node_sum;max_sum=in1.max_sum;max_node=in1.max_node}
  else {nsum=node_sum;max_sum=in2.max_sum;max_node=in2.max_node}


let rec max_interior = function 
| Ln x -> {nsum=x;max_sum=x;max_node=""}
|In {key=k;l_t=Ln l; r_t=Ln t} ->{nsum=l+t;max_sum=l+t;max_node=k}
|In {key=k;l_t=In tree; r_t=Ln leaf} | In {key=k;l_t=Ln leaf; r_t=In tree} ->let res=(max_interior (In tree)) in  max_leaf_interior k leaf res
|In {key=k;l_t=In l_tree; r_t=In r_tree} -> let in1 = (max_interior (In l_tree)) in 
                                            let in2 = (max_interior (In r_tree)) in 
                                            max_interior_interior k in1 in2

let res2=max_interior tree2                                            
let res3=max_interior tree3

let ()=
printf "\n\n max node is %s, max_sum is %i \n\n" res3.max_node res3.max_sum;
printf "\n\n max node is %s, max_sum is %i \n\n" res2.max_node res2.max_sum


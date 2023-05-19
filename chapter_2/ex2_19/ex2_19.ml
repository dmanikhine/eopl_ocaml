open Base 
open Stdio
(* open Base.Poly *)

type bintree=
| Leave
| Node of {top: int; l_t: bintree; r_t:bintree}

let number_to_bintree num = 
   Node {top=num;l_t=Leave; r_t=Leave;}

let insert_to_left num bt=
match bt with
| Leave -> number_to_bintree num
| Node {top=n;l_t=l; r_t=r;} -> Node {
                                top=n; 
}


| Leave -> Node ({top=t;
                  l_t=Node({top=num;l_t=Leave;r_t=Leave});
                  r_t=r})
| Node x ->Node ({top=t;
                  l_t=Node({top=num;l_t=Node x;r_t=Leave;});
                  r_t=r;})
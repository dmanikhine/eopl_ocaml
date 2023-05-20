open Base 
open Stdio
(* open Base.Poly *)

type bintree=
| Ln of int (*Leaf node *)
| In of {key:int;l_t:bintree;r_t:bintree} (*Interior node *)






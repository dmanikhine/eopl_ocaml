(* open Base *)

type expval=
|Num_val of int

(*
type expression =
|Const_exp of int
|Diff_exp of expression * expression
|If_exp of expression * expression * expression
|Var_exp of char
|Let_exp of char * expression
*)

let num_to_expval var_num = Num_val var_num
let expval_to_num = function
|Num_val x -> x
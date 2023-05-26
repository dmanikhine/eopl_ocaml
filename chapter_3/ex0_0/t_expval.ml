(* open Base *)

type expval=
|Bool_val of bool 
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
let bool_to_expval var_bool = Bool_val var_bool
let expval_to_num = function
|Num_val x -> x
|Bool_val _ -> failwith "\n Error in function expval_to_num! bool is not int \n"

let expval_to_bool = function
|Bool_val x -> x
|Num_val _ -> failwith "\n Error in function expval_to_bool! int is not bool! \n"







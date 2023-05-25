(* open Base *)

type expval=
|Bool of bool 
|Int of int

(*
type expression =
|Const_exp of int
|Diff_exp of expression * expression
|If_exp of expression * expression * expression
|Var_exp of char
|Let_exp of char * expression
*)

let num_to_expval var_int = Int var_int
let bool_to_expval var_bool = Bool var_bool
let expval_to_num = function
|Int x -> x
|Bool _ -> failwith "\n Error in function expval_to_num! bool is not int \n"

let expval_to_bool = function
|Bool x -> x
|Int _ -> failwith "\n Error in function expval_to_bool! int is not bool! \n"







open T_expression
open T_environment




type expval=
|Bool_val of bool 
|Num_val of int
|Proc_val of char*expression*env



let num_to_expval var_num = Num_val var_num
let bool_to_expval var_bool = Bool_val var_bool
let expval_to_num = function
|Num_val x -> x
|_ -> failwith "\n Error in function expval_to_num!\n"

let expval_to_bool = function
|Bool_val x -> x
|_ -> failwith "\n Error in function expval_to_bool!\n"







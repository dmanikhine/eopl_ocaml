(* open Base *)
open Stdio
open T_expval
open T_environment
open T_expression



let cond1=make_ZeroQ_exp (make_Const_exp 10) 
let cnst1=make_Const_exp 100
let cond2=make_ZeroQ_exp (make_Const_exp 0) 
let cnst2=make_Const_exp 200


let cond_exp =make_Cond_exp [(cond1,cnst1);(cond2,cnst2)]
let env=make_Empty_env



let () =
printf "\n variable cond_exp has value %i \n" (expval_to_num (value_of cond_exp env));

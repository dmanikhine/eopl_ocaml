(* open Base *)
open Stdio
open T_expval
open T_environment
open T_expression



let a=make_Const_exp 7
let b=make_Const_exp 8
let equalQ_ab=make_EqualQ_exp a b 
let greaterQ_ab=make_GreaterQ_exp a b 
let lessQ_ab=make_LessQ_exp a b 

let env=make_Empty_env



let () =
printf "\n variable equalQ_ab has value %b\n" (expval_to_bool (value_of equalQ_ab env));
printf "\n variable greaterQ_ab has value %b\n" (expval_to_bool (value_of greaterQ_ab env));
printf "\n variable lessQ_ab has value %b\n" (expval_to_bool (value_of lessQ_ab env));
(* open Base *)
open Stdio
open T_expval
open T_environment
open T_expression



let a=make_Const_exp 7
let b=make_Const_exp 8

let add_ab=make_Add_exp a b
let mult_ab=make_Mult_exp a b
let div_ba=make_Div_exp b a
let env=make_Empty_env


let () =
printf "\n variable add_ab has value %i\n" (expval_to_num (value_of add_ab env));
printf "\n variable mult_ab has value %i\n" (expval_to_num (value_of mult_ab env));
printf "\n variable div_ba has value %i\n" (expval_to_num (value_of div_ba env));  
(* open Base *)
open Stdio
open T_expval
open T_environment
open T_expression



let a=make_Const_exp 5
let minus_a=make_Minus_exp a
let b=make_Const_exp 9
let diff_ab=make_Diff_exp minus_a b
let result=make_Minus_exp diff_ab
let env=make_Empty_env


let () =
printf "\n variable result has value %i\n" (expval_to_num (value_of result env));

  
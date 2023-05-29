(* open Base *)
open Stdio
open T_expval
open T_environment
open T_expression


let body0=make_Diff_exp (make_Var_exp 'x') (make_Var_exp 'y')
let exp1=make_Diff_exp (make_Var_exp 'x') (make_Const_exp 1)
let exp2=make_Diff_exp (make_Var_exp 'x') (make_Const_exp 2)

let body1=make_Let_exp [('x',exp1);('y',exp2)] body0

let test_exp=make_Let_exp [('x',make_Const_exp 30)] body1
let env=make_Empty_env

let () =
printf "\n var test_exp has value %i\n" (expval_to_num (value_of test_exp env))
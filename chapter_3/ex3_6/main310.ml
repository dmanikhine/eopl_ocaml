(* open Base *)
open Stdio
open T_expval
open T_environment
open T_expression


let var=make_Var_exp 'x'
let var_value=make_Const_exp 4
let cnst1=make_Const_exp 1
let cnst2=make_Const_exp 3

let diff1= make_Diff_exp var cnst1
let diff2= make_Diff_exp var cnst2

let lst=make_List_exp [var;diff1;diff2]

let let_exp =make_Let_exp 'x' var_value lst
let env=make_Empty_env



let () =
printf "\n variable let_exp has value \n"; print_Cons_val (value_of let_exp env);

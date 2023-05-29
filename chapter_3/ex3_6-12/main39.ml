(* open Base *)
open Stdio
open T_expval
open T_environment
open T_expression


let var=make_Var_exp 'x'
let var_value=make_Const_exp 4
let const_value=make_Const_exp 1
let diff= make_Diff_exp var const_value
let emptylist=make_Emptylist_exp
let cons1=make_Cons_exp diff emptylist

let cons2 =make_Cons_exp cons1 emptylist
let cons3 = make_Cons_exp var cons2
let let_exp =make_Let_exp 'x' var_value cons3
let env=make_Empty_env



let () =
printf "\n variable v_car has value \n"; print_Cons_val (value_of let_exp env);

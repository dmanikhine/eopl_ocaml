(* open Base *)
open Stdio
open T_expval
open T_environment
open T_expression

let expval1=num_to_expval 20
let env1=make_env 'a' expval1 (make_empty_env)
let expval2=bool_to_expval true
let env2=make_env 'b' expval2 env1

let exp1=make_Const_exp 100
let exp2=make_Const_exp 200
let exp0=make_ZeroQ_exp (make_Const_exp 10)

let expif=make_If_exp exp0 exp1 exp2


let () =
printf "\n var a has value %i\n" (expval_to_num (apply_env env1 'a'));
printf "\n var b has value %b\n" (expval_to_bool (apply_env env2 'b'));
printf "\n var z has binding %b\n" (has_bindingQ env2 'z');
printf "\n exp1 has value %i\n" (expval_to_num (value_of exp1 env2));
printf "\n expif has value %i\n" (expval_to_num (value_of expif env2))

  
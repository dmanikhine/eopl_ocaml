open T_expval
open T_environment


type expression =
|Const_exp of int
|ZeroQ_exp of expression
|If_exp of expression * expression * expression
|Diff_exp of expression * expression
|Var_exp of char
(*|Let_exp of char * expression *)

let make_Const_exp n = Const_exp n
let make_ZeroQ_exp exp =ZeroQ_exp exp
let make_Diff_exp exp1 exp2 = Diff_exp(exp1,exp2)
let make_Var_exp c =Var_exp c
let make_If_exp exp1 exp2 exp3 =If_exp(exp1,exp2,exp3)



let rec value_of exp env=
match exp with
|Const_exp n -> num_to_expval n
|Var_exp var -> apply_env env var
|Diff_exp (exp1, exp2) -> num_to_expval (  (expval_to_num (value_of exp1 env)) - (expval_to_num (value_of exp2 env)))
|ZeroQ_exp exp1 -> if expval_to_num(value_of exp1 env)=0 then bool_to_expval true
                   else bool_to_expval false
|If_exp (exp1,exp2,exp3) -> if (expval_to_bool (value_of exp1 env)) then value_of exp2 env
else value_of exp3 env


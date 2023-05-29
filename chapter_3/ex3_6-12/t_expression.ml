open Base
open T_expval
open T_environment


type expression =
|Const_exp of int
|ZeroQ_exp of expression
|If_exp of expression * expression * expression
|Diff_exp of expression * expression
|Var_exp of char
|Let_exp of char * expression * expression
|Minus_exp of expression
|Add_exp of expression * expression
|Mult_exp of expression * expression
|Div_exp of expression * expression
|EqualQ_exp of expression * expression
|GreaterQ_exp of expression * expression
|LessQ_exp of expression * expression
|Emptylist_exp
|Cons_exp of expression * expression
|Car_exp of expression
|Cdr_exp of expression
|NullQ_exp of expression
|List_exp of expression list
|Cond_exp of (expression * expression) list  


let make_Const_exp n = Const_exp n
let make_ZeroQ_exp exp =ZeroQ_exp exp
let make_Diff_exp exp1 exp2 = Diff_exp(exp1,exp2)
let make_Var_exp c =Var_exp c
let make_If_exp exp1 exp2 exp3 =If_exp(exp1,exp2,exp3)
let make_Let_exp c exp1 body = Let_exp(c,exp1,body)
let make_Minus_exp exp=Minus_exp exp
let make_Add_exp exp1 exp2 = Add_exp (exp1, exp2)
let make_Mult_exp exp1 exp2 = Mult_exp (exp1, exp2)
let make_Div_exp exp1 exp2 = Div_exp (exp1, exp2)
let make_EqualQ_exp exp1 exp2 = EqualQ_exp (exp1, exp2)
let make_GreaterQ_exp exp1 exp2 = GreaterQ_exp (exp1, exp2)
let make_LessQ_exp exp1 exp2 = LessQ_exp (exp1, exp2)
let make_Emptylist_exp =Emptylist_exp
let make_Cons_exp exp1 exp2 = Cons_exp (exp1,exp2)
let make_Car_exp exp1 = Car_exp exp1
let make_Cdr_exp exp1 = Cdr_exp exp1
let make_NullQ_exp exp1 =NullQ_exp exp1
let make_List_exp lst =List_exp lst
let make_Cond_exp lst_cond_exp =Cond_exp lst_cond_exp


let rec value_of exp env=
match exp with
|Const_exp n -> num_to_expval n
|Var_exp var -> apply_env env var
|Diff_exp (exp1, exp2) -> num_to_expval (  (expval_to_num (value_of exp1 env)) - (expval_to_num (value_of exp2 env)))
|ZeroQ_exp exp1 -> if expval_to_num(value_of exp1 env)=0 then bool_to_expval true
                   else bool_to_expval false
|If_exp (exp1,exp2,exp3) -> if (expval_to_bool (value_of exp1 env)) then value_of exp2 env
                    else value_of exp3 env
|Let_exp (var,exp1,body) -> value_of body (make_Extend_env var (value_of exp1 env) env)                  
|Minus_exp exp1 -> num_to_expval (-(expval_to_num (value_of exp1 env)))
|Add_exp(exp1, exp2) -> num_to_expval((expval_to_num (value_of exp1 env)) + (expval_to_num (value_of exp2 env)))
|Mult_exp(exp1, exp2) -> num_to_expval((expval_to_num (value_of exp1 env)) * (expval_to_num (value_of exp2 env)))
|Div_exp(exp1, exp2) -> num_to_expval((expval_to_num (value_of exp1 env)) / (expval_to_num (value_of exp2 env)))
|EqualQ_exp(exp1,exp2)->bool_to_expval((expval_to_num (value_of exp1 env)) = (expval_to_num (value_of exp2 env)))
|GreaterQ_exp(exp1,exp2)->bool_to_expval((expval_to_num (value_of exp1 env)) > (expval_to_num (value_of exp2 env)))
|LessQ_exp(exp1,exp2)->bool_to_expval((expval_to_num (value_of exp1 env)) < (expval_to_num (value_of exp2 env)))
|Emptylist_exp -> make_Emptylist_val
|Cons_exp(exp1,exp2) ->make_Cons_val (value_of exp1 env) (value_of exp2 env)
|Car_exp exp1 -> expval_to_car (value_of exp1 env)
|Cdr_exp exp1 -> expval_to_cdr (value_of exp1 env)
|NullQ_exp exp1 -> emptylistQ (value_of exp1 env)
|List_exp x ->  let aux aux_env aux_val=value_of aux_val aux_env in 
                list_val (List.map ~f:(aux env) x)
|Cond_exp lst_cond_exp -> let rec cond_aux = function
                              |[] -> failwith "value-of All cond tests failed!"
                              |(x,y)::t -> if(expval_to_bool(value_of x env)) then value_of y env 
                              else cond_aux t in 
                              cond_aux lst_cond_exp
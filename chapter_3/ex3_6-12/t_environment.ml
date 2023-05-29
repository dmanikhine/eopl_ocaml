(*open Base *)
(* open Base.Poly *)
open T_expval

type env =
|Empty_env
|Extend_env of char * expval * env

let make_Empty_env=Empty_env
let make_Extend_env var var_val env =Extend_env(var,var_val,env)

let rec apply_env search_env search_var =
   match search_env with 
|Empty_env -> failwith ("functon apply_env No binding for " ^ String.make 1 search_var)
|Extend_env (c,value,env)-> if (Char.equal c search_var) then value
else (apply_env env search_var)

let rec has_bindingQ search_env search_var= 
match search_env with
|Empty_env -> false
|Extend_env (c,_,env)-> if (Char.equal c search_var) then true
else (has_bindingQ env search_var)


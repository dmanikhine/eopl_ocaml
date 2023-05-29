open T_expval

type env =
|Empty_env
|Extend_env of (char * expval) list * env

let make_Empty_env=Empty_env
let make_Extend_env var_val_lst env =Extend_env(var_val_lst,env)

let rec apply_env search_var search_env=
   match search_env with 
|Empty_env -> failwith ("functon apply_env No binding for " ^ String.make 1 search_var)
|Extend_env (var_val_lst1,env1)-> let rec aux = function
                                  |[] -> apply_env search_var env1
                                  |(var1,val1)::t -> if (Char.equal var1 search_var) then val1
                                                    else aux t in 
                                  aux var_val_lst1


let rec has_bindingQ search_var search_env= 
match search_env with
|Empty_env -> false
|Extend_env (var_val_lst1,env1)-> let rec aux = function
                                  |[] -> has_bindingQ search_var env1
                                  |(var1,_)::t -> if (Char.equal var1 search_var) then true
                                                    else aux t in 
                                  aux var_val_lst1


let is_empty_envQ = function
| Empty_env -> true
| _ -> false


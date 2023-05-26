open Base 
open Stdio
(* open Base.Poly *)

type env =
|Empty_env
|Extend_env of char * int * env

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

let env1=Extend_env('a',20,Empty_env)
let env2=Extend_env('b',10,env1)


let ()=printf "\n var a has value %i\n" (apply_env env2 'a');
printf "\nvar z has binding %b\n" (has_bindingQ env2 'z')


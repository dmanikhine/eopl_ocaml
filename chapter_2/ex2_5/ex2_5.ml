open Base
open Stdio
open Base.Poly


type elmEnv =
Elm of char*int 

type env = Env of elmEnv list
let empty_env=[]

let extend_env evar eval env =
  Elm(evar,eval)::env

let rec apply_env search_var = function
| [] -> None
| Elm(evar, eval)::tail ->
   if (evar=search_var) then Some(eval)
   else apply_env search_var tail

let () =
 let v_in='s' in 
 let v_check='e' in
 let test_env=(extend_env v_in 5 empty_env) in
 let res=apply_env v_check test_env in 
 match res with
  | None -> printf "No binding for %c \n" v_check 
  | Some eval -> printf "%i\n" eval




      


open Base
open Stdio
open Base.Poly


type elmEnv =
Elm of char*int 

type env = Env of elmEnv list
let empty_env=[]

let is_empty_env = function
| [] -> printf "true"; true
| _::_ -> printf "false"; false


let extend_env evar eval env =
  Elm(evar,eval)::env

let rec apply_env search_var = function
| [] -> None
| Elm(evar, eval)::tail ->
   if (evar=search_var) then Some(eval)
   else apply_env search_var tail

let () =
 let res=is_empty_env empty_env in
 printf "The end of programm. Result is %b \n" res




      


open Base 
open Stdio
(* open Base.Poly *)
type env={
   env_fun: char->int option;
   env_status: unit->bool;
}

let empty_env = {
   env_fun=(fun search_var ->printf "No binding found for %c \n" search_var; None);
   env_status=(fun ()->true)
}

(* let (apply_env : (char -> int option)-> char-> int option)=
fun env search_var -> (env search_var) *)

let apply_env {env_fun=_env_fun; env_status=_} search_var = (_env_fun search_var)

let extend_env saved_var saved_val saved_env =
   {env_fun=(fun search_var -> 
      if (Char.equal search_var saved_var) then (Some saved_val)
      else (apply_env saved_env search_var));
      env_status=(fun()->false);
   }
 
let is_empty_env {env_fun=_;env_status=_env_status}=
_env_status

let new_env1=extend_env 's' 100 empty_env
let new_env2=extend_env 'a' 200 new_env1


let ()= printf "\nEnvironment is empty: %b \n\n" (is_empty_env new_env1 ());
let v=apply_env new_env2 'a' in 
match v with
| Some(x) -> printf "\n%i\n" x
| _ -> printf "\nThe end \n"


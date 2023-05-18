open Base 
open Stdio
(* open Base.Poly *)


let (empty_env : char -> int option)=
 fun search_var ->
   printf "No binding found for %c \n" search_var; None 

(* let (apply_env : (char -> int option)-> char-> int option)=
fun env search_var -> (env search_var) *)

let apply_env env search_var = (env search_var)

 let extend_env saved_var saved_val saved_env =
  fun search_var -> 
    if (Char.equal search_var saved_var) then (Some saved_val)
    else (apply_env saved_env search_var)

let new_env1=extend_env 's' 100 empty_env
let new_env2=extend_env 'a' 200 new_env1


let ()= let v=apply_env new_env2 'a' in 
match v with
| Some(x) -> printf "\n%i\n" x
| _ -> printf "\nThe end \n"


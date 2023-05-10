 open Base 
open Stdio
(* open Base.Poly *)

let empty_env _ =
  printf "no binding found";
  None

 let apply_env env search_var =
(env search_var)

let extend_env (evar : 'a) eval env =
  let f search_var =
    if (Char.equal evar search_var) then Some(eval)
    else (apply_env env search_var) in 
    f


let () =
let env1 = extend_env 'a' 10 empty_env in 
let env2 = extend_env 'f' 25 env1 in
match (apply_env env2 's') with
| None -> printf ""
| Some(x) -> printf "\n %i \n" x

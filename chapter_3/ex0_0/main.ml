(* open Base *)
open Stdio
open T_expval
open T_environment

let expval1=num_to_expval 20
let env1=make_env 'a' expval1 (make_empty_env)
let expval2=bool_to_expval true
let env2=make_env 'b' expval2 env1




let () =
printf "\n var a has value %i\n" (expval_to_num (apply_env env1 'a'));
printf "\n var b has value %b\n" (expval_to_bool (apply_env env2 'b'))
(*printf "\nvar z has binding %b\n" (has_bindingQ env2 'z') *)

  
(* open Base *)
open Stdio
open T_expval
open T_environment

let empty_env=make_Empty_env
let env1=make_Extend_env [('a',num_to_expval 100)] empty_env 

let () =
printf "\n var a has value %i\n" (expval_to_num (apply_env 'a' env1));

  
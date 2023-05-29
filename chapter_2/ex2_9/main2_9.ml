open Stdio
open T_environment

 
let env1=make_Extend_env 'a'1 make_Empty_env

let () =
 let res=has_bindingQ 'b' env1 in
 printf "Result is %b \n" res  


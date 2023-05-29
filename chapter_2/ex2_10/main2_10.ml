open Stdio
open T_environment

 
let env1=make_Extend_env [('a',100);('b',200)] make_Empty_env


let () =
 printf "Variable b has binding: %b \n" (has_bindingQ 'b' env1);
 printf "Variable b value is: %i \n" (apply_env 'b' env1)  


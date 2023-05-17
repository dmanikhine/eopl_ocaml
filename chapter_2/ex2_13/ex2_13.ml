open Base 
open Stdio
(* open Base.Poly *)


let (empty_env : char -> int option)=
 fun search_var ->
   printf "No binding found %c" search_var; None 


let (apply_env : (char -> int option)-> char-> int option)=
fun env search_var -> (env search_var)


let ()= printf "test test test"


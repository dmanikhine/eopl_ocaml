open Base 
open Stdio
(* open Base.Poly *)

type rec_stk = {
  status: bool;
  el_value: unit->int option
}

let empty_stack =
let f1 () = printf "TOP Stack is empty"; Some(5) in 
 {
status= false;
el_value= f1;
} 


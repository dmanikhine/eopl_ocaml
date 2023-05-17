open Base 
open Stdio
(* open Base.Poly *)


type rec_stk = {
  status: bool;
  el_value: (unit->int option);
  old_stk: (unit -> rec_stk option)
}
 


let empty_stack =
 {
status= true;
el_value= (fun ()-> printf "TOP Stack is empty"; Some 20);
old_stk =(fun ()-> printf "TOP Stack is empty"; None);
} 

let is_empty_stack {status= stk_status;el_value=_;}=
stk_status




let ()= printf "\n %b \n\n" (is_empty_stack empty_stack)
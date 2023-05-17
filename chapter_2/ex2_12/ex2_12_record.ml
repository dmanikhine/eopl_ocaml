open Base 
open Stdio
(* open Base.Poly *)


type rec_stk = {
  status: bool;
  el_value: (unit->int option);
  old_stk: (unit -> rec_stk option)
}
 
let push e stk =
  {
    status = false;
    el_value = (fun ()->Some e);
    old_stk= (fun () -> Some stk);
  }

let pop {status=status;el_value=el_value;old_stk=old_stk}=
match old_stk() with
|None -> printf "POP Stack is empty"; {status=status;el_value=el_value;old_stk=old_stk}
|Some x -> x


let empty_stack =
{
status= true;
el_value= (fun ()-> None);
old_stk =(fun ()-> None);
} 




let top {status= _;el_value=el_value;old_stk=_} =
  match el_value() with
| Some i -> printf "\n %i \n" i
| None -> printf "\n TOP Stack is empty \n\n"



let is_empty_stack {status= stk_status;el_value=_;old_stk=_}=
stk_status

let new1_stk=(push 100 empty_stack)  
let new2_stk=(push 200 new1_stk)  

let old1_stk=(pop new2_stk)

let ()= printf "\n %b \n\n" (is_empty_stack empty_stack);
top empty_stack;
top new1_stk;
top new2_stk;
top old1_stk;

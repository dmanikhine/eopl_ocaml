open Base 
open Stdio
(* open Base.Poly *)

type f1 =
|Funit of (unit->unit)
|Fint of (unit->int)

type f2 =
|Funit of (unit->unit)
|Fstk of (unit->rec_stk)
and
rec_stk = {
  status: bool;
  el_value: (unit->f1);
  old_stk: f2;
}

let push e stk =


let empty_stack =
 {
status= true;
el_value= Funit(fun ()-> printf "TOP Stack is empty");
old_stk = Funit(fun ()-> printf "POP Stack is empty");
} 

let is_empty_stack {status= stk_status;el_value=_; old_stk=_}=
stk_status




let ()= printf "\n %b \n\n" (is_empty_stack empty_stack)
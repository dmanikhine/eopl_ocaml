(* open Base *)
open Stdio
(* open Base.Poly *)


type element =
| BoolElement of bool
| UnitIntElement of (unit->int)
| FunctionElement of (unit -> unit)
| ListElement of element list

let empty_stack = 
let f0 = BoolElement(true) in
let f1 ()= printf "TOP Stack is empty" in
let f2 ()= printf "POP Stack is empty" in
[f0;FunctionElement(f1);FunctionElement(f2)]

let push e stk =
  let f1 () =e in
[BoolElement(false);UnitIntElement(f1);ListElement(stk)]

let top = function
| _::UnitIntElement(x)::_ -> x ()
| _ -> raise(Failure "top")

let pop = function
|_::_::ListElement(x)::_ -> x
| _ -> raise (Failure "pop")

let is_empty_stack = function
| BoolElement (x)::_ -> x 
| _ -> raise (Failure "is_empty_stack")

let () = printf "%i \n" (top (pop (push 10 (push 5 empty_stack))))



      


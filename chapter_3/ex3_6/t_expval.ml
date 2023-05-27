(* open Base *)
open Stdio

type expval=
|Bool_val of bool 
|Num_val of int
|Emptylist_val
|Cons_val of expval*expval

let make_Emptylist_val =Emptylist_val
let make_Cons_val expval1 expval2 = Cons_val (expval1,expval2)

let rec print_Cons_val  = function
|Bool_val _ | Num_val _ -> failwith "Error in funciton print_cons_val! Operand is not Cons_val!"
|Emptylist_val -> printf " emptylist "
|Cons_val (h,t)-> match h with 
                  | Cons_val (_,_) | Emptylist_val -> printf " ( "; print_Cons_val h; printf " ) ";print_Cons_val t
                  | Bool_val x -> printf " %b " x; print_Cons_val t
                  | Num_val x -> printf " %i " x; print_Cons_val t
              


let num_to_expval var_num = Num_val var_num
let bool_to_expval var_bool = Bool_val var_bool
let expval_to_num = function
|Num_val x -> x
|_  -> failwith "\n Error in function expval_to_num! value is not Num_val \n"


let expval_to_bool = function
|Bool_val x -> x
|_ -> failwith "\n Error in function expval_to_bool! value is not Bool_val! \n"

let expval_to_car =function
|Cons_val (head,_) -> head
|Emptylist_val -> failwith "\n Empty list has no car! \n" 
|_ -> failwith "\n Error in function expval_to_car! value has no car! \n"

let expval_to_cdr =function
|Cons_val (_,tail) -> tail
|Emptylist_val -> failwith "\n Empty list has no cdr! \n" 
|_ -> failwith "\n Error in function expval_to_cdr! value has no cdr! \n"

let emptylistQ = function
|Emptylist_val -> Bool_val true
|Cons_val(_,_) -> Bool_val false
|_ -> failwith "\n Error in function emptylistQ! value is not list! \n"



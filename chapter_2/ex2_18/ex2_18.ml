open Base 
open Stdio
(* open Base.Poly *)

type numberInSequence = {
   top: int;
   left_l: int list;
   right_l: int list;
   }


let number_to_sequence element_number={
   top=element_number;
   left_l=[];
   right_l=[];
   }

let current_element nis=
nis.top

let move_to_left nis=
match nis.left_l with
|[] -> failwith "left_l is empty!"
|h::s -> {top=h; left_l=s;right_l=nis.top::nis.right_l}

let move_to_right nis=
match nis.right_l with
|[] -> failwith "right_l is empty!"
|h::s -> {top=h; left_l=nis.top::nis.left_l;right_l=s}

let insert_to_left elm nis ={
   top=nis.top;
   left_l=elm::nis.left_l;   
   right_l=nis.right_l;
   }

   let insert_to_right elm nis ={
      top=nis.top;
      left_l=nis.left_l;   
      right_l=elm::nis.right_l;
      }
   
let nis1=number_to_sequence 7

let nis2=insert_to_left 6 nis1
let nis3 insert_to_right 8 nis1



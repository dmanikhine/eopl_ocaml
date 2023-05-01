open Base
open Stdio 
(*open Base.Poly*)

let rec walk_list s counter= function
| [] -> (s,counter)
| head::tail -> walk_list (s ^ String.make 1 head) (counter+1) tail
  

let () = 
let (s,counter)=walk_list "" 0 ['a';'b';'c';'d'] in 
printf "s: %s \n" s;
printf"counter: %i \n" counter;

      

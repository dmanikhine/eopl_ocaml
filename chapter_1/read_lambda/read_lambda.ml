open Base
open Stdio 
open Base.Poly


type lcExp =
  | One of char
  | Lambda of char
  | Many of lcExp list

  let reverse l =
    let rec aux reversed = function
      | [] -> reversed
      | Many(x)::tail -> aux (Many(aux [] x)::reversed) tail 
      | head :: tail -> aux (head::reversed) tail in
    aux [] l


let explode_string s =
  let rec exp i lcExp_list =
    if i < 0 then lcExp_list else exp (i - 1) (One(s.[i]) :: lcExp_list) in
  exp (String.length s - 1) []

let rec take_lambda acc = function  
  | [] -> (acc,[])
  | Many(_)::tail -> take_lambda acc tail
  | One(l)::One(a1)::One(m)::One(b)::One(d)::One(a2)::One(_)::One(_)::One(x)::One(_)::tail when (Char.equal l 'l'
                                        && Char.equal a1 'a'
                                        && Char.equal m 'm'
                                        && Char.equal b 'b'
                                        && Char.equal d 'd'
                                        && Char.equal a2 'a') ->  take_lambda (Lambda(x)::acc) tail 
 | One(parentheses_open) :: tail when (Char.equal parentheses_open '(')-> let (racc,rlst)= (take_lambda [] tail) in
                                                  take_lambda (Many(racc)::acc) rlst 
 | One(parentheses_close) :: tail when (Char.equal parentheses_close ')')->  (acc, tail)
 | head::tail -> take_lambda (head::acc) tail
  

let () = 
  let rec aux = function
      | [] -> printf "\n"
      | One(x)::tail -> printf "%c " x; aux tail
      | Lambda(x) :: tail -> printf "lambda (%c) " x; aux tail
      | Many(x):: tail -> printf "\n("; aux x; printf ")\n"; aux tail in 
    let (racc,_)=(take_lambda [] (explode_string "lambda (y) (lambda (z) (x (y z)))")) in
    aux (reverse racc)

    (*  aux (explode_string "x y lampda x lambda y") *)

    (*aux [One('b');Many([One('a');One('b');One('c')])]  *)
    (*aux (take_lambda [] (explode_string "x y lampda x lambda y"))*)

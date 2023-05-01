open Base
open Stdio 
open Base.Poly


type lcExp =
  | One of char
  | Many of lcExp list
  | LambdaC of char * char
  | LambdaL of char * lcExp list


  let reverse l =
    let rec aux reversed = function
      | [] -> reversed
      | Many(x)::tail -> aux (Many(aux [] x)::reversed) tail 
      | LambdaL(x,expression)::tail -> aux ((LambdaL(x, aux [] expression))::reversed) tail 
      | head :: tail -> aux (head::reversed) tail in
    aux [] l


let explode_string s =
  let rec exp i lcExp_list =
    if i < 0 then lcExp_list else exp (i - 1) (s.[i] :: lcExp_list) in
  exp (String.length s - 1) []


  let rec aux = function
  | [] -> printf ""
  | One(x)::tail -> printf "%c" x; aux tail
  | LambdaL(x,expression) :: tail -> printf "\n(lambda (%c) (" x; aux expression; printf "l)"; aux tail
  | LambdaC(x,symbol) :: tail -> printf "\n(lambda (%c) %c)" x symbol; aux tail
  | Many(x):: tail -> printf "\n(m"; aux x; printf "m)\n"; aux tail 


let rec take_lambda acc = function  
  | [] -> (acc,[])
  | _::l::a1::m::b::d::a2::_::_::x::_::_::parentheses_open::tail when (Char.equal l 'l'
                                        && Char.equal a1 'a'
                                        && Char.equal m 'm'
                                        && Char.equal b 'b'
                                        && Char.equal d 'd'
                                        && Char.equal a2 'a'
                                        && Char.equal parentheses_open '('
                                        ) -> let (racc,rlst)= (take_lambda [] (parentheses_open::tail)) in
                                                  take_lambda (LambdaL(x,racc)::acc) rlst 
  | _::l::a1::m::b::d::a2::_::_::x::_::_::s::_::tail when (Char.equal l 'l'
                                        && Char.equal a1 'a'
                                        && Char.equal m 'm'
                                        && Char.equal b 'b'
                                        && Char.equal d 'd'
                                        && Char.equal a2 'a') -> take_lambda (LambdaC(x,s)::acc) tail 
  | parentheses_open :: tail when (Char.equal parentheses_open '(')-> let (racc,rlst)= (take_lambda [] tail) in
                                                  take_lambda (Many(racc)::acc) rlst 
  | parentheses_close :: tail when (Char.equal parentheses_close ')')->  (acc, tail)
  | head::tail -> take_lambda (One(head)::acc) tail


let print1 = 
  let rec aux = function
      | [] -> printf ""
      | One(x)::tail -> printf "%c" x; aux tail
      | LambdaL(x,expression) :: tail -> printf "\n(lambda (%c)" x; aux expression; printf ")\n"; aux tail
      | LambdaC(x,symbol) :: tail -> printf "\n(lambda (%c) %c)" x symbol; aux tail
      | Many(x):: tail -> printf "\n("; aux x; printf ")\n"; aux tail in 
    let (racc,_)=(take_lambda [] (explode_string "((lambda (x) x) (x y))")) in
    aux (reverse racc)

let () = print1



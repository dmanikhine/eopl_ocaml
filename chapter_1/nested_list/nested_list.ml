open Base
open Stdio 
(* open Base.Poly *)


let reverse l =
  let rec aux reversed = function
    | [] -> reversed
    | head :: tail -> aux (head::reversed) tail in
  aux [] l


type flatLcExp =
|FOne of char
|FLambda of char

type lcExp =
  | One of char
  | Lambda of char
  | Many of lcExp list

let flatten l =
    let rec aux acc = function
      | [] -> acc
      | One head :: tail ->  aux (FOne(head) :: acc) tail
      | Lambda(x) :: tail -> aux(FLambda(x)::acc) tail
      | Many l :: tail -> aux (aux acc l) tail in
    aux [] l

let rec aux  = function
        | [] -> printf " \n"
        | FOne(x) :: tail -> printf "%c" x; aux tail 
        | FLambda(x):: tail -> printf "lambda %c" x; aux tail                      

let () = aux (flatten [ One 'a' ; Many [ One 'b' ; Many [ One 'c' ; One 'd' ] ; One 'e' ]; Lambda('z')])
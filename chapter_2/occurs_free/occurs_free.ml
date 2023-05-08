open Base
open Stdio
open Base.Poly


type lcExp =
  | Var_exp of char  
  | Lambda_exp  of char * lcExp
  | App_exp of lcExp * lcExp
  

let v = (Lambda_exp('x',App_exp(Var_exp('a'),App_exp(Var_exp('b'),Var_exp('c')))))


let () = 
  let (racc,_)=(take_lambda [] (explode_string "(lambda (y) (lambda (z) (x (y z))))")) in
  if(occurs_free 'x' racc) then printf "\n true \n\n"
  else printf "\n false \n\n";
  let (racc,_)=(take_lambda [] (explode_string "(lambda (x) (x y))")) in
  if(occurs_free 'x' racc) then printf "\n true \n\n"
  else printf "\n false \n\n"
      


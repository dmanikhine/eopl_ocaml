open Base
open Stdio
(* open Base.Poly *)


type lcExp =
  | Var_exp of char  
  | Lambda_exp  of char * lcExp
  | App_exp of lcExp * lcExp

let v1 = Var_exp('x')  
let v2 = Var_exp('y')  
let v3 = Lambda_exp('x',App_exp(Var_exp('x'),Var_exp('y')))
let v4 = Lambda_exp('y',App_exp(Var_exp('x'),Var_exp('y')))

let v5 = App_exp((Lambda_exp('x',Var_exp('x'))),App_exp(Var_exp('x'),Var_exp('y')))

let v6 = Lambda_exp('y',
          Lambda_exp ('z',
            App_exp(Var_exp('x'),
              App_exp(Var_exp('y'),Var_exp('z')))))



let is_var_exp = function
| Var_exp(_)-> true
|_-> false

let is_lambda_exp = function
| Lambda_exp(_)-> true
|_-> false

let is_app_exp = function
| App_exp(_)-> true
|_-> false

let varexp_to_var = function
| Var_exp(x)-> x
|_-> ' '

let lambdaexp_to_bound_var = function
| Lambda_exp(x,_)-> x
|_-> ' '

let lambdaexp_to_body = function
| Lambda_exp(_,x)-> x
|_-> Var_exp(' ')

let appexp_to_rator = function
| App_exp(x,_)-> x
|_-> Var_exp(' ')

let appexp_to_rand = function
| App_exp(_,y)-> y
|_-> Var_exp(' ')


let rec is_occurs_free search_var exp= 
if (is_var_exp exp) then (Char.equal search_var (varexp_to_var exp))
else if (is_lambda_exp exp) then
   ((not (Char.equal search_var (lambdaexp_to_bound_var exp))) && 
   (is_occurs_free search_var (lambdaexp_to_body exp)))
else (
  (is_occurs_free search_var (appexp_to_rand exp)) ||
  (is_occurs_free search_var (appexp_to_rator exp)) 
)

let () =
printf "\n v1 %b \n" (is_occurs_free 'x' v1);
printf "\n v2 %b \n" (is_occurs_free 'x' v2);
printf "\n v3 %b \n" (is_occurs_free 'x' v3);
printf "\n v4 %b \n" (is_occurs_free 'x' v4);
printf "\n v5 %b \n" (is_occurs_free 'x' v5);
printf "\n v5 %b \n" (is_occurs_free 'x' v6);
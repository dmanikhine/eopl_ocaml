open Base
open Stdio
open Base.Poly


type lcExp =
  | Char_lcExp of char 
  | String_lcExp of string

type loc_lolc=
{ret_loc: char list;
ret_lolc: string list;
}
  
let is_symbol exp = 
  match exp with
| Char_lcExp(_)   -> true
| String_lcExp(_) -> false


let explode_lcExp_string s =
  let rec exp i lcExp_list =
    if i < 0 then lcExp_list else exp (i - 1) (s.[i] :: lcExp_list) in
      exp (String.length s - 1) []


let rec add_to_list loc lolc = 
  match loc with 
  | c_end::tl when c_end=')' ->{ret_loc=tl; ret_lolc=lolc;}
  | c_begin::tl when c_begin='(' ->
   let ret= add_to_list tl [] in
      {ret_loc=ret.ret_loc; ret_lolc=lolc::ret.ret_lolc};
  |





let loc_to_lolc loc lolc =  
  match loc with
  | [] -> []
  | c_begin::tl when c_begin='(' -> add_to_list tl []
  | c_space::tl when c_space=' ' -> f_help1 tl lolc
  | c_end::tl when c_end=')' -> f_help1 tl lolc
  | c_identifier::c_space::tl when c_space=' ' -> f_help1 tl c_identifier::lolc





  


  





let() =
let predicat= is_symbol (Char_lcExp 'x') in
 printf "%b\n" predicat

open Base
open Stdio

let list_to_string lst =
  String.concat ~sep:"; " lst

let rec loc_to_string_help s loc=
  match loc with
    | [] -> s
    | hd :: tl ->
       loc_to_string_help (s ^ String.make 1 hd) tl

let loc_to_string  loc =
loc_to_string_help "" loc 


let rec remove_first_v2 s los =
  match los with
  | [] -> []
  | hd :: tl -> 
    if Char.equal hd s then remove_first_v2 s tl
    else hd::(remove_first_v2 s tl)


let() =
    let loc=remove_first_v2 'a' ['a';'b';'c';'d';'e';'f';'a'] in
     let str=loc_to_string loc in
     printf "%s\n" str

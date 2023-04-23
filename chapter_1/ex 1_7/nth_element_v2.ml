open Base
open Stdio

let list_to_string lst =
  String.concat ~sep:"; " lst

let report_list_to_short lst n =   
  printf "list ["; 
  printf "%s" (list_to_string lst);
  printf "] does not have %i elements \n" (n+1)
  

let rec nth_element_helper input_list n current_list i =
  if List.is_empty current_list then begin
    report_list_to_short input_list n;
    None
  end
  else if n=0 then Some (List.hd_exn current_list)
  else nth_element_helper input_list n (List.tl_exn current_list) (i-1)  


let nth_element input_list n =
  nth_element_helper input_list n input_list n


let() =
  printf "5 element is: %s\n"
    (match  (nth_element ["OCaml";"Perl";"C";"test";"test";"Aisha"] 10) with
        | None -> ""
        | Some m -> m
    )


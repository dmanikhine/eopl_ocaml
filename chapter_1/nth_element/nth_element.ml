open Base
open Stdio

let report_list_to_short n =
printf "list to short by %i element \n" (n+1)

  

let rec nth_element lst n =
if List.is_empty lst then begin
  report_list_to_short n;
  None
end
else if n=0 then Some (List.hd_exn lst)
else nth_element (List.tl_exn lst) (n-1)

let() =
  printf "5 element is: %s\n"
    (match  (nth_element ["OCaml";"Perl";"C";"test";"test";"Aisha"] 10) with
        | None -> ""
        | Some m -> m
    )


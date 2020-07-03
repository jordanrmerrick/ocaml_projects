open Base
open Stdio

let cc_validation ccnum =
  let int_to_char_list n =
    String.to_list (Int.to_string n)
  in
  if List.length (int_to_char_list ccnum) = 16 then
    let rec find_eo ls =
      match ls with
      | [] | [_] -> []
      | _::m::l -> m::find_eo l
    in
    let charls_to_intls ls =
      List.map ~f:(Char.to_int) ls
    in
    let sum_and_check ls =
      List.map ~f:(( * ) 2) ls
    in
    sum_and_check (charls_to_intls (find_eo (int_to_char_list ccnum)))
  else []

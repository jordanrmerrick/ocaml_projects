open Base

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
    let charls_to_strls ls =
      List.map ~f:(Char.to_string) ls
    in
    let strls_to_intls ls =
      List.map ~f:(Int.of_string) ls
    in
    let sum_and_check ls =
      List.map ~f:(( * ) 2) ls
    in
    let rec check_dd ls =
      match ls with
      | [] | [_] -> []
      | f::s ->
        if f >= 10 then
          Char.to_string (Int.to_string
        else []

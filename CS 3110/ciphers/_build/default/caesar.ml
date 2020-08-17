open Base

let caesar text shift =
  (* The easiest *)
  let char_list s =
    String.to_list s
  in
  let char_to_ascii ls =
    List.map ~f:(Char.to_int) ls
  in
  let rec shift_check s i =
    if i >= 65 && i <= 90 then
      if i + s >= 90 then
        if i + 26 >= 65 then i else shift_check (i-26) s
      else i + s
    else if i >= 97 && i <= 122 then
      if i + s >= 122 then
        if i + 26 > 97 then i else shift_check (i-26) s
      else i + s
    else
      0
  in
  let shift_chars ls shift =
    List.map ~f:(shift_check shift) ls
  in
  let asciimap_to_string ls =
    let i_to_c ls =
      List.map ~f:(Char.of_int_exn) ls
    in
    let c_to_s ls =
      List.map ~f:(Char.to_string) ls
    in
    String.concat (c_to_s (i_to_c ls))
  in
  asciimap_to_string (shift_chars (char_to_ascii (char_list text)) shift)

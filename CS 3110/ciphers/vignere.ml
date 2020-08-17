open Base

let key_translation keystr =
  let str_to_list keystr =
    String.to_list keystr
  in
  let key_values k =
    match k with
    | 'A' | 'a' -> 0
    | 'B' | 'b' -> 1
    | 'C' | 'c' -> 2
    | 'D' | 'd' -> 3
    | 'E' | 'e' -> 4
    | 'F' | 'f' -> 5
    | 'G' | 'g' -> 6
    | 'H' | 'h' -> 7
    | 'I' | 'i' -> 8
    | 'J' | 'j' -> 9
    | 'K' | 'k' -> 10
    | 'L' | 'l' -> 11
    | 'M' | 'm' -> 12
    | 'N' | 'n' -> 13
    | 'O' | 'o' -> 14
    | 'P' | 'p' -> 15
    | 'Q' | 'q' -> 16
    | 'R' | 'r' -> 17
    | 'S' | 's' -> 18
    | 'T' | 't' -> 19
    | 'U' | 'u' -> 20
    | 'V' | 'v' -> 21
    | 'W' | 'w' -> 22
    | 'X' | 'x' -> 23
    | 'Y' | 'y' -> 24
    | 'Z' | 'z' -> 25
    | _ -> 0
  in
  List.map ~f:(key_values) (str_to_list keystr)

let plaintext_translation plaintext =
  let uppercase_str plaintext =
    String.uppercase plaintext
  in
  let str_to_list plaintext =
    String.to_list plaintext
  in
  List.map ~f:(Char.to_int) (str_to_list (uppercase_str plaintext))

let asciiL_to_string intList =
  let asciiL_to_charL intList =
    List.map ~f:(Char.of_int_exn) intList
  in
  let charL_to_string charList =
    let charL_to_stringL charList=
      List.map ~f:(String.of_char) charList
    in
    String.concat (charL_to_stringL charList)
  in
  charL_to_string (asciiL_to_charL intList)

let vignere plaintext cipher =
  let pt = plaintext_translation plaintext in
  let ct = key_translation cipher in
  let merged_ascii p k = List.map2_exn ~f:(fun x y -> x+y) p k
  in
  asciiL_to_string (merged_ascii pt ct)

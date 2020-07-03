open Core

(* if n%3=0 then "Fizz" else if n%5=0 then "Buzz" else if n%5=0&&n%3=0 then "FizzBUzz" else "%i"(n)*)

let fizzbuzz n =
  if n%3=0 && n%5=0 then begin
    printf "FizzBuzz\n"
  end
  else if n%3=0 then begin
    printf "Fizz\n"
  end
  else if n%5=0 then begin
    printf "Buzz\n"
  end
  else printf "%s\n"(Int.to_string n)

(* Testing

let () = fizzbuzz 15
let () = fizzbuzz 6
let () = fizzbuzz 10
let () = fizzbuzz 13
*)

(* Factorial func *)

let rec factorial n =
  match n with
  | 0 -> 1
  | 1 -> 1
  | x -> x * factorial (x-1)

(* Testing
let () = printf "%i\n" (factorial 0)
let () = printf "%i\n" (factorial 1)
let () = printf "%i\n" (factorial 3)
let () = printf "%i\n" (factorial 6)
*)

(* reverses each character to its inverse component and then reverses the string *)
let rna_reverse s =
  let reverse s =
    match s with
    | 'A' -> 'U'
    | 'U' -> 'A'
    | 'G' -> 'C'
    | 'C' -> 'G'
    | _ -> 'P'
  in
  String.of_char_list (List.rev (List.map ~f:reverse (String.to_list (s))))

(*let () = printf "%s\n" (rna_reverse "CAGGU")*)

(* turns radians into degrees *)

let to_degree f =
  (* 2 *. Float.pi = 360 *)
  f *. (180. /. Float.pi)
       
(* Testing to_degree *)
       
(*       
let () =
  for i=1 to 360 do
    printf "%f\n" (to_degree (Float.pi /. (float_of_int i)))
*)  

(*
This function sums the individual digits of a number (58 -> 5 + 8 -> 13) and continues to sum the resulting numbers until it is a single digit number. The function
should return the number of times this operation has been completed.

Example 1. 58 -> 5+8 -> 13 -> 1+3 -> 4 -> 2
Example 2. 14 -> 1 + 4 -> 5 -> 1
Example 3. 3 -> 3 -> 0
*)
let persistence n =
  let int_to_int_list n =
    let int_to_slist n =
      List.map ~f:String.of_char (String.to_list (Int.to_string n))
    in
    List.map ~f:Int.of_string (int_to_slist n)
  in
  let rec check_l ls =
     match ls with
      | first :: rest -> first + check_l rest
      | [] -> 0
  in
  let rec check_multiple_elements ls count =
    if (List.length ls) > 1 then begin
      check_multiple_elements (int_to_int_list (check_l ls)) (count+1)
    end
    else
      count
  in
  check_multiple_elements (int_to_int_list n) 0

(* Assertion tests with persistence. Will throw an exception if it's incorrect.*)
let () =
  let temp = persistence 56 in
  assert (temp = 2)
let () =
  let temp = persistence 14 in
  assert (temp = 1)
let () = let temp = persistence 3 in
  assert (temp = 0)

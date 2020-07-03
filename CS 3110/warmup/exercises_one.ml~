open Base
open Stdio

let multiply_n a b =
  a *. b


let divide_n a b =
  a /. b

(** square_f returns the square of a float *)
let square_f a =
  a *. a

(** exp returns an exponent of any given float, provided the exponent is an int. For the exercises this will return 4.2 ^ 7. *)
let rec exp n e =
  match (e / 2 + 1) with
  | 0 -> square_f n *. 1.
  | 1 -> square_f n *. n
  | x -> square_f n *. exp n (x - 1)

let evaluation n1 n2 =
  if n1 > n2 then 42 else 7

let double n =
  n * n
  
let cubed_f n =
  n *. n *. n

let circle_area r =
  Float.pi *. square_f r

let int_sign i =
  if i > 1 then 1
  else if i = 0 then 0
  else -1

let rms x y =
  Float.sqrt ((square_f x +. square_f y) /. 2.)

let valid_date y m d =
  if y > 0 then
    match m with
    | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> if d >= 1 && d <= 31 then true else false
    | "Apr" | "Jun" | "Sep" | "Nov" -> if d >= 1 && d <= 30 then true else false
    | "Feb" ->
      if y%400=0 then
        if d >=1 && d <= 29 then true else false
      else if y%100=0 then
        if d >=1 && d <= 28 then true else false
      else if y%4=0 then
        if d >=1 && d <= 29 then true else false
      else
      if d >= 1 && d <= 28 then true else false
    | _ -> false
  else false

let () = printf "\n\n-----------valid_date tests------------\n\n" 
(* Should return false - valid year and month, but 2021 is not a leap year so max day is 28*)
let () = printf "false -> %b\n" (valid_date 2021 "Feb" 29)
(* Should return true - leap year *)
let () = printf "true -> %b\n" (valid_date 2020 "Feb" 29)
(* Should return true *)
let () = printf "true -> %b\n" (valid_date 2016 "Apr" 30)
(* Should return true *)
let () = printf "true -> %b\n" (valid_date 2018 "Oct" 31)
(* Should return false - bad month str*)
let () = printf "false -> %b\n" (valid_date 2019 "Octt" 31)
(* Should return true *)
let () = printf "true -> %b\n" (valid_date 2018 "Oct" 14)
(* Should return false - date out of range *)
let () = printf "false -> %b\n" (valid_date 1914 "Sep" 31)


let syr n =
  let rec aux n t =
    match n with
    | 1 -> t
    | x -> if x%2 = 0 then aux (x/2) (t+1) else aux (x*3+1) (t+1)
  in
  aux n 0

let () = printf "\n\n-----------syracuse tests-----------\n\n"
let () = printf "%i\n" (syr 10)
let () = printf "%i\n" (syr 4)
let () = printf "%i\n" (syr 147)

let nacci k n =
  
  let rec solver ls n =
    if n > 0 then
    match ls with
    | [] -> 1
    | [x] -> x
    | first :: rest -> first + solver rest (n-1)
    else 0 in
  
  let rec helper n k prev pprev =
    if prev <= k then helper n k (prev+1) ((solver pprev n)::pprev)
    else pprev
  in
  List.rev (helper n k 1 [])

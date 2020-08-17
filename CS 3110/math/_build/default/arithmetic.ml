open Base
open Stdio

let rec sum ls =
  match ls with
  | [] -> 0
  | first::rest -> first + sum rest

let () = printf "%i\n" (sum [1;2;3;4;5])

let rec exp n e =
  let square_f a =
    a *. a
  in
  match (e / 2) with
  | 0 -> square_f n *. 1.
  | 1 -> square_f n *. n
  | x -> square_f n *. exp n (x-1)

let () = printf "%f\n" (exp 2. 6)

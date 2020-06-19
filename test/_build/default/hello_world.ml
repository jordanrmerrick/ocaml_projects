open Base
open Stdio

let rec sum ls =
  match ls with
  | [] -> 0
  | first :: rest -> first + sum rest


let () =
  printf "Total: %i\n" (sum [1;2;3;4])

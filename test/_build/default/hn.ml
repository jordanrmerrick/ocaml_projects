open Base

let rec sum ls =
  match ls with
  | [] -> 0
  | first :: rest -> first + sum rest

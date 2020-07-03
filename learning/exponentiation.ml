open Base
open Stdio

let rec sum ls =
  match ls with
  | [] -> 0
  | first :: rest -> first + sum rest

let exponent num exp =
  let x = 0 in
    while exp > x do
      let numx = num in
        numx = num * num
                 exp - 1
               
    done
    
let () =
  printf "%i\n" (exponent 2 2)


open Base
open Stdio

let fast_fib n =
  let rec helper n prev pprev =
    match n with
    | 1 -> prev
    | x -> helper (x-1) (prev+pprev) prev
  in
  helper (n+1) 0 1

let () = printf "The Nth fibonacci number is %i\n" (fast_fib 50)

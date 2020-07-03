open Core

let rec fib n =
  match n with
  | 0 -> 1
  | 1 -> 1
  | x -> fib (x-1) + fib (x-2)

let fast_fib n =
  let rec helper n prev pprev =
    match n with
    | 0 -> 1
    | 1 -> 1
    | x -> helper (x-1) (prev+pprev) prev
  in
  helper n 0 1

let rec factorial n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | x -> x * factorial (x-1)

let rec exponent n k =
  match k with
  | 0 -> 1
  | 1 -> n
  | x -> n * exponent n (x-1)


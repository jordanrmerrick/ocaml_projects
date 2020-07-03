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

let int_to_roman n =
  let x = [] in
  let rec helper n ls =
    if n >= 1000 then begin
      ls@["M"],
      helper (n-1000) x
    end
    else if n>=500 && n<=1000 then begin
        ls@["D"],
        helper (n-500) x
      end
    else if n>=100 && n<=500 then begin
      ls@["C"],
      helper (n-100) x
    end
    else if n>=50 && n<=100 then begin
      ls@["L"],
      helper (n-50) x
    end
    else if n>=10 && n<=50 then begin
      ls@["X"],
      helper (n-10) x
    end
    else if n>=5 && n<=10 then begin
      ls@["V"],
      helper (n-5) x
    end
    else if n>=1 && n<= 5 then begin
      ls@["I"],
      helper (n-1) x
    end
    else x
  in
  helper n []

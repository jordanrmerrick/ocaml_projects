open Core
open! Test

(*
let () =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"empty"
      (fun () -> ());
    Bench.Test.create ~name:"fib 5"
      (fun () -> fib 5);
    Bench.Test.create ~name:"fib 10"
      (fun () -> fib 10);
    Bench.Test.create ~name:"fib 25"
      (fun () -> fib 25);
    Bench.Test.create ~name:"fast_fib 5"
      (fun () -> fast_fib 5);
    Bench.Test.create ~name:"fast_fib 10"
      (fun () -> fast_fib 10);
    Bench.Test.create ~name:"fast_fib 25"
      (fun () -> fast_fib 25);
    Bench.Test.create  ~name:"fast_fib 200"
      (fun () -> fast_fib 200)
  ])

let () = printf "%i\n" (factorial 10)
*)

let () = printf "%i\n" (exponent 4 7)

# 1 "book/duniverse/ocaml-migrate-parsetree.1.5.0/src/stdlib0.ml"
module Int = struct
  let to_string = string_of_int
end

module Option = struct
  let map f o =
    match o with
    | None -> None
    | Some v -> Some (f v)
end


let () =
  Hashtbl.add Toploop.directive_table "require" (Toploop.Directive_string ignore);
  Hashtbl.add Toploop.directive_table "use" (Toploop.Directive_string (fun _ ->
    failwith "#use is not allowed inside a dune file in OCaml syntax"));
  Hashtbl.add Toploop.directive_table "use_mod" (Toploop.Directive_string (fun _ ->
    failwith "#use is not allowed inside a dune file in OCaml syntax"))

module Jbuild_plugin = struct
  module V1 = struct
    let context       = "default"
    let ocaml_version = "4.09.0"

    let ocamlc_config =
      [ "version"                   , "4.09.0"
      ; "standard_library_default"  , "/Users/jordanmerrick/.opam/default/lib/ocaml"
      ; "standard_library"          , "/Users/jordanmerrick/.opam/default/lib/ocaml"
      ; "standard_runtime"          , "the_standard_runtime_variable_was_deleted"
      ; "ccomp_type"                , "cc"
      ; "c_compiler"                , "cc"
      ; "ocamlc_cflags"             , "-O2 -fno-strict-aliasing -fwrapv"
      ; "ocamlopt_cflags"           , "-O2 -fno-strict-aliasing -fwrapv"
      ; "bytecomp_c_compiler"       , "cc -O2 -fno-strict-aliasing -fwrapv -D_FILE_OFFSET_BITS=64 -D_REENTRANT"
      ; "bytecomp_c_libraries"      , "-lm -lpthread"
      ; "native_c_compiler"         , "cc -O2 -fno-strict-aliasing -fwrapv -D_FILE_OFFSET_BITS=64 -D_REENTRANT"
      ; "native_c_libraries"        , "-lm"
      ; "cc_profile"                , ""
      ; "architecture"              , "amd64"
      ; "model"                     , "default"
      ; "int_size"                  , "63"
      ; "word_size"                 , "64"
      ; "system"                    , "macosx"
      ; "asm"                       , "clang -arch x86_64 -Wno-trigraphs -c"
      ; "asm_cfi_supported"         , "true"
      ; "with_frame_pointers"       , "false"
      ; "ext_exe"                   , ""
      ; "ext_obj"                   , ".o"
      ; "ext_asm"                   , ".s"
      ; "ext_lib"                   , ".a"
      ; "ext_dll"                   , ".so"
      ; "os_type"                   , "Unix"
      ; "default_executable_name"   , "a.out"
      ; "systhread_supported"       , "true"
      ; "host"                      , "x86_64-apple-darwin18.7.0"
      ; "target"                    , "x86_64-apple-darwin18.7.0"
      ; "profiling"                 , "false"
      ; "flambda"                   , "false"
      ; "spacetime"                 , "false"
      ; "safe_string"               , "false"
      ; "exec_magic_number"         , "Caml1999X026"
      ; "cmi_magic_number"          , "Caml1999I026"
      ; "cmo_magic_number"          , "Caml1999O026"
      ; "cma_magic_number"          , "Caml1999A026"
      ; "cmx_magic_number"          , "Caml1999Y026"
      ; "cmxa_magic_number"         , "Caml1999Z026"
      ; "ast_impl_magic_number"     , "Caml1999M026"
      ; "ast_intf_magic_number"     , "Caml1999N026"
      ; "cmxs_magic_number"         , "Caml1999D026"
      ; "cmt_magic_number"          , "Caml1999T026"
      ; "natdynlink_supported"      , "true"
      ; "supports_shared_libraries" , "true"
      ; "windows_unicode"           , "false"
      ]

    let send s =
      let oc = open_out_bin "../../../../_build/.dune/default/book/duniverse/stdlib-shims.0.1.0/src/dune" in
      output_string oc s;
      close_out oc

    let run_and_read_lines cmd =
      let tmp_fname = Filename.temp_file "dune" ".output" in
      at_exit (fun () -> Sys.remove tmp_fname);
      let n =
        Printf.ksprintf Sys.command "%s > %s" cmd (Filename.quote tmp_fname)
      in
      let rec loop ic acc =
        match input_line ic with
        | exception End_of_file -> close_in ic; List.rev acc
        | line -> loop ic (line :: acc)
      in
      let output = loop (open_in tmp_fname) [] in
      if n = 0 then
        output
      else begin
        Printf.ksprintf failwith
          "Command failed: %s\n\
           Exit code: %d\n\
           Output:\n\
           %s"
          cmd n (String.concat "\n" output)
      end
  end
end
# 1 "book/duniverse/stdlib-shims.0.1.0/src/dune"
(* -*- tuareg -*- *)

open StdLabels
open Jbuild_plugin.V1

let version = Scanf.sscanf ocaml_version "%u.%u" (fun a b -> (a, b))

let modules_in_4_02 =
  [ "Arg"
  ; "Array"
  ; "ArrayLabels"
  ; "Buffer"
  ; "Bytes"
  ; "BytesLabels"
  ; "Callback"
  ; "Char"
  ; "Complex"
  ; "Digest"
  ; "Filename"
  ; "Format"
  ; "Gc"
  ; "Genlex"
  ; "Hashtbl"
  ; "Int32"
  ; "Int64"
  ; "Lazy"
  ; "Lexing"
  ; "List"
  ; "ListLabels"
  ; "Map"
  ; "Marshal"
  ; "MoreLabels"
  ; "Nativeint"
  ; "Obj"
  ; "Oo"
  ; "Parsing"
  ; "Pervasives"
  ; "Printexc"
  ; "Printf"
  ; "Queue"
  ; "Random"
  ; "Scanf"
  ; "Set"
  ; "Stack"
  ; "StdLabels"
  ; "Stream"
  ; "String"
  ; "StringLabels"
  ; "Sys"
  ; "Weak"
  ]

let modules_post_4_02 =
  [ "Float", (4, 07)
  ; "Seq", (4, 07)
  ; "Stdlib", (4, 07)
  ; "Uchar", (4, 03)
  ]

let available_modules =
  modules_in_4_02 @
  (List.filter modules_post_4_02 ~f:(fun (m, v) ->
       version >= v)
   |> List.map ~f:fst)

let all_modules_except_stdlib =
  available_modules
  |> List.filter ~f:((<>) "Stdlib")
  |> List.sort ~cmp:String.compare

let longest_module_name =
  List.fold_left all_modules_except_stdlib ~init:0
    ~f:(fun acc m -> max acc (String.length m))

let stdlib_rule =
  Printf.sprintf {|
(rule
 (with-stdout-to stdlib.ml
  (echo "\
%s

include Pervasives
")))
|}
    (List.map all_modules_except_stdlib
       ~f:(fun m -> Printf.sprintf "module %-*s = %s" longest_module_name m m)
     |> String.concat ~sep:"\n")

let () =
  Printf.ksprintf send {|
(library
 (wrapped false)
 (name stdlib_shims)
 (public_name stdlib-shims))
%s
|}
    (if version >= (4, 07) then "" else stdlib_rule)

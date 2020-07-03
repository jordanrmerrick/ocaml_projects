
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
      let oc = open_out_bin "../../../../_build/.dune/default/book/duniverse/bigarray-compat.1.0.0/src/dune" in
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
# 1 "book/duniverse/bigarray-compat.1.0.0/src/dune"
(* -*- tuareg -*- *)

open Jbuild_plugin

type t = { major: int; minor: int; patch: int option; extra: string option }
let v ?patch ?extra major minor = { major; minor; patch; extra }

let major { major; _ } = major
let minor { minor; _ } = minor
let patch { patch; _ } = patch
let extra { extra; _ } = extra

let parse s =
  try Scanf.sscanf s "%d.%d.%d+%s" (fun major minor patch extra -> v ~patch ~extra major minor)
  with End_of_file | Scanf.Scan_failure _ -> begin
      try Scanf.sscanf s "%d.%d+%s" (fun major minor extra -> v ~extra major minor)
      with End_of_file | Scanf.Scan_failure _ -> begin
          try Scanf.sscanf s "%d.%d.%d" (fun major minor patch -> v ~patch major minor)
          with End_of_file | Scanf.Scan_failure _ -> begin
              Scanf.sscanf s "%d.%d" (fun major minor -> v major minor)
            end
        end
end

let ocaml_version = V1.ocaml_version |> parse

let has_bigarray_in_stdlib = major ocaml_version >= 4 && minor ocaml_version >= 7
let base_dune = "(library (name bigarray_compat) (public_name bigarray-compat) (modules bigarray_compat) (wrapped false)"

let dune_file_stdlib = base_dune^") (rule (targets bigarray_compat.ml) (action (copy bigarray_stdlib.ml bigarray_compat.ml)))"
let dune_file_pre407 = base_dune^"(libraries bigarray)) (rule (targets bigarray_compat.ml) (action (copy bigarray_pre407.ml bigarray_compat.ml)))"

let _ = match has_bigarray_in_stdlib with
    | true -> V1.send dune_file_stdlib
    | false -> V1.send dune_file_pre407
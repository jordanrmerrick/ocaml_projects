open Core
open Async

let query_uri query =
  let static = Uri.of_string "http://api.duckduckgo.com/?format=json" in
  Uri.add_query_param static ("q", [query])

let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list ->
    let find key =
      begin match List.Assoc.find ~equal:String.equal kv_list key with
        | None | Some (`String "") -> None
        | Some s -> Some (Yojson.Safe.to_string s)
      end
    in
    begin match find "Abstract" with
      | Some _ as x -> x
      | None -> find "Definition"
    end
  | _ -> None

let get_definition word =
  Cohttp_async.Client.get (query_uri word)
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body
  >>| fun string ->
  (word, get_definition_from_json string)

let print_result (word,definition) =
  printf "%s\n%s\n\n%s\n\n"
    word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
     | None -> "no definition found"
     | Some def ->
       String.concat ~sep:"\n"
         (Wrapper.wrap (Wrapper.make 70) def))

let search_and_print words =
  Deferred.all_unit (List.map words ~f:(fun word ->
      get_definition word >>| print_result))

let () =
  Command.async_spec
    ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Spec.(
      empty
      +> anon (sequence ("word" %: string))
    )
    (fun words () -> search_and_print words)
  |> Command.run
       


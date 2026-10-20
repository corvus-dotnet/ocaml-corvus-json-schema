(* A Bowtie (https://github.com/bowtie-json-schema/bowtie) harness for the OCaml package corvus-json-schema.

   It speaks IHOP (one JSON request per line on standard input, one response per line on standard output):
   - start reports the implementation and its dialects;
   - dialect sets the dialect for schemas without $schema;
   - run compiles the case's schema with the case's registry as the document resolver and validates each instance
     (for annotations output, through a verbose collector, reporting each annotation with its instance location and
     #... keyword location);
   - stop exits.

   Requests are read with Yojson.Raw, which keeps each number and string as Bowtie wrote it, so schemas and instances
   reach the package unchanged (numbers included). *)

module C = Corvus_json_schema

let dialects =
  [
    ("https://json-schema.org/draft/2020-12/schema", C.Draft2020_12);
    ("https://json-schema.org/draft/2019-09/schema", C.Draft2019_09);
    ("http://json-schema.org/draft-07/schema#", C.Draft7);
    ("http://json-schema.org/draft-06/schema#", C.Draft6);
    ("http://json-schema.org/draft-04/schema#", C.Draft4);
  ]

let fail message =
  prerr_endline message;
  exit 1

let member name = function `Assoc fields -> List.assoc_opt name fields | _ -> None

(* A string as Bowtie wrote it, decoded. *)
let string_value = function
  | Some (`Stringlit literal) -> (
      match Yojson.Safe.from_string literal with `String s -> Some s | _ -> None)
  | _ -> None

let string_literal s = `Stringlit (Yojson.Safe.to_string (`String s))

let without_fragment uri = match String.index_opt uri '#' with Some i -> String.sub uri 0 i | None -> uri

(* As the other Corvus harnesses encode a keyword in a #... keyword location. *)
let percent_encoded value =
  let allowed = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~!$&'()*+,;=:@/?" in
  let b = Buffer.create (String.length value) in
  String.iter
    (fun c -> if String.contains allowed c then Buffer.add_char b c else Printf.bprintf b "%%%02X" (Char.code c))
    value;
  Buffer.contents b

(* A JSON pointer token unescaped: ~1 is /, ~0 is ~. *)
let unescaped_token token =
  let b = Buffer.create (String.length token) in
  let n = String.length token in
  let rec go i =
    if i < n then
      if token.[i] = '~' && i + 1 < n && (token.[i + 1] = '1' || token.[i + 1] = '0') then (
        Buffer.add_char b (if token.[i + 1] = '1' then '/' else '~');
        go (i + 2))
      else (
        Buffer.add_char b token.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

let file_text path =
  try
    let ic = open_in path in
    let line = try input_line ic with End_of_file -> "" in
    close_in ic;
    String.trim line
  with Sys_error _ -> ""

let start () =
  let optional name value = if value = "" then [] else [ (name, string_literal value) ] in
  `Assoc
    [
      ("version", `Intlit "1");
      ( "implementation",
        `Assoc
          ([
             ("language", string_literal "ocaml");
             ("name", string_literal "corvus-json-schema");
             ("version", string_literal C.version);
             ("homepage", string_literal "https://github.com/corvus-dotnet/Corvus.JsonSchema");
             ("documentation", string_literal "https://github.com/corvus-dotnet/corvus-json-schema-ocaml");
             ("issues", string_literal "https://github.com/corvus-dotnet/Corvus.JsonSchema/issues");
             ("source", string_literal "https://github.com/corvus-dotnet/Corvus.JsonSchema");
             ("dialects", `List (List.map (fun (uri, _) -> string_literal uri) dialects));
           ]
          @ optional "os" (file_text "/proc/sys/kernel/ostype")
          @ optional "os_version" (file_text "/proc/sys/kernel/osrelease")
          @ [ ("language_version", string_literal ("OCaml " ^ Sys.ocaml_version)) ]) );
    ]

let errored message = [ ("errored", `Bool true); ("context", `Assoc [ ("message", string_literal message) ]) ]

(* The annotations a verbose collector grouped (instance location, then keyword as a JSON pointer token, then schema
   location as a #... fragment; the same in every Corvus implementation) as Bowtie lists them: each with its keyword
   unescaped, and its keyword location the schema location's fragment followed by / and the keyword token,
   percent-encoded as the fragment is. *)
let annotations collector =
  `List
    (List.concat_map
       (fun (instance_location, by_keyword) ->
         List.concat_map
           (fun (keyword, by_location) ->
             List.map
               (fun (location, value) ->
                 `Assoc
                   [
                     ("keyword", string_literal (unescaped_token keyword));
                     ("instanceLocation", string_literal instance_location);
                     ("keywordLocation", string_literal (location ^ "/" ^ percent_encoded keyword));
                     ("annotation", Yojson.Raw.from_string value);
                   ])
               by_location)
           by_keyword)
       (C.Results.collect_annotations collector))

let run dialect request =
  let seq = Option.value (member "seq" request) ~default:`Null in
  let case = Option.value (member "case" request) ~default:`Null in
  let registry =
    match member "registry" case with
    | Some (`Assoc entries) -> List.map (fun (uri, schema) -> (without_fragment uri, Yojson.Raw.to_string schema)) entries
    | _ -> []
  in
  let resolver uri =
    match List.assoc_opt (without_fragment uri) registry with
    | Some json -> Result.to_option (C.Document.parse_string json)
    | None -> None
  in
  let annotate = string_value (member "output" request) = Some "annotations" in
  let schema = match member "schema" case with Some s -> Yojson.Raw.to_string s | None -> "" in
  let options = C.options ~default_dialect:dialect ~resolver () in
  let body =
    try
      match C.Validator.compile_string ~options schema with
      | Error e -> errored (C.error_to_string e)
      | Ok validator ->
          let collector = C.Results.create C.Results.Verbose in
          let tests = match member "tests" case with Some (`List tests) -> tests | _ -> [] in
          let result test =
            match member "instance" test with
            | None -> `Assoc (errored "the test has no instance")
            | Some instance -> (
                let instance = Yojson.Raw.to_string instance in
                if not annotate then
                  match C.Validator.validate_string validator instance with
                  | Ok valid -> `Assoc [ ("valid", `Bool valid) ]
                  | Error e -> `Assoc (errored (C.error_to_string e))
                else (
                  C.Results.reset collector;
                  match C.Validator.evaluate_string validator ~results:collector instance with
                  | Ok valid -> `Assoc [ ("valid", `Bool valid); ("annotations", annotations collector) ]
                  | Error e -> `Assoc (errored (C.error_to_string e))))
          in
          [ ("results", `List (List.map result tests)) ]
    with e -> errored (Printexc.to_string e)
  in
  `Assoc (("seq", seq) :: body)

let () =
  let started = ref false in
  let dialect = ref C.Draft2020_12 in
  try
    while true do
      let line = input_line stdin in
      if String.trim line <> "" then begin
        let request = try Yojson.Raw.from_string line with Yojson.Json_error m -> fail ("A request is not JSON: " ^ m) in
        let response =
          match string_value (member "cmd" request) with
          | None -> fail "A request has no command"
          | Some "start" ->
              if member "version" request <> Some (`Intlit "1") then fail "Unsupported IHOP version";
              started := true;
              start ()
          | Some cmd when not !started -> fail ("Not started: " ^ cmd)
          | Some "dialect" -> (
              match Option.bind (string_value (member "dialect" request)) (fun uri -> List.assoc_opt uri dialects) with
              | Some d ->
                  dialect := d;
                  `Assoc [ ("ok", `Bool true) ]
              | None -> `Assoc [ ("ok", `Bool false) ])
          | Some "run" -> run !dialect request
          | Some "stop" -> exit 0
          | Some cmd -> fail ("Unknown command: " ^ cmd)
        in
        print_endline (Yojson.Raw.to_string response)
      end
    done
  with End_of_file -> ()

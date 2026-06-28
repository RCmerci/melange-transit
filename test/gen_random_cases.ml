open Transit_core.Json

module Gen = QCheck2.Gen

let atom =
  Gen.oneof
    [
      Gen.pure Null;
      Gen.map (fun value -> Bool value) Gen.bool;
      Gen.map (fun value -> String value)
        (Gen.oneof_list [ ""; "plain"; "~tilde"; "^caret"; "`tick"; "hello" ]);
      Gen.map (fun value -> Int value) (Gen.int_range (-10_000) 10_000);
      Gen.map (fun value -> Int64 value)
        (Gen.oneof_list
           [
             2_147_483_648L;
             -2_147_483_649L;
             9_007_199_254_740_991L;
             9_007_199_254_740_992L;
           ]);
      Gen.map (fun value -> Float value)
        (Gen.oneof_list [ -10.5; -0.25; 1.25; 10.5 ]);
      Gen.map (fun value -> Binary value)
        (Gen.oneof_list [ ""; "hi"; "\000\001\002"; "bytes" ]);
      Gen.map (fun value -> Keyword value)
        (Gen.oneof_list [ "a"; "ab"; "color"; "block/created-at" ]);
      Gen.map (fun value -> Symbol value)
        (Gen.oneof_list [ "s"; "sym"; "thing"; "ns/name" ]);
      Gen.map (fun value -> Big_decimal value)
        (Gen.oneof_list [ "0.0"; "1.20"; "123.456" ]);
      Gen.map (fun value -> Big_int value)
        (Gen.oneof_list [ "0"; "123"; "12345678901234567890" ]);
      Gen.map (fun value -> Date value)
        (Gen.oneof_list [ 0L; 1L; 123_456_789L; 1_777_063_533_827L ]);
      Gen.pure (Uuid "531a379e-31bb-4ce1-8690-158dceb64be6");
      Gen.pure (Uri "https://example.com/path?q=1");
    ]

let rec value depth =
  if depth <= 0 then atom
  else
    Gen.oneof_weighted
      [
        (7, atom);
        (3, Gen.map (fun values -> Array values) (small_values depth));
        (2, Gen.map (fun values -> List values) (small_values depth));
        (2, Gen.map (fun values -> Set values) (unique_set_values ()));
        (3, Gen.map (fun values -> keyed_map values) (small_values depth));
        (1, Gen.map (fun values -> Tagged ("point", Array values)) (small_values depth));
      ]

and small_values depth =
  let open Gen in
  let* size = int_range 0 3 in
  list_size (pure size) (value (depth - 1))

and unique_set_values () =
  let open Gen in
  let* size = int_range 0 3 in
  pure (List.init size (fun index -> String ("set-" ^ string_of_int index)))

and keyed_map values =
  Map
    (List.mapi
       (fun index value -> (String ("key-" ^ string_of_int index), value))
       values)

let escape_ocaml_string text = Printf.sprintf "%S" text

let rec value_expr = function
  | Null -> "c.null"
  | Bool value -> Printf.sprintf "c.bool %b" value
  | String value -> Printf.sprintf "c.string %s" (escape_ocaml_string value)
  | Int value -> Printf.sprintf "c.int (%d)" value
  | Int64 value -> Printf.sprintf "c.int64 (%LdL)" value
  | Float value -> Printf.sprintf "c.float (%s)" (Float.to_string value)
  | Binary value -> Printf.sprintf "c.binary %s" (escape_ocaml_string value)
  | Keyword value -> Printf.sprintf "c.keyword %s" (escape_ocaml_string value)
  | Symbol value -> Printf.sprintf "c.symbol %s" (escape_ocaml_string value)
  | Big_decimal value ->
      Printf.sprintf "c.big_decimal %s" (escape_ocaml_string value)
  | Big_int value -> Printf.sprintf "c.big_int %s" (escape_ocaml_string value)
  | Date value -> Printf.sprintf "c.date (%LdL)" value
  | Uuid value -> Printf.sprintf "c.uuid %s" (escape_ocaml_string value)
  | Uri value -> Printf.sprintf "c.uri %s" (escape_ocaml_string value)
  | Array values ->
      Printf.sprintf "c.array [ %s ]"
        (String.concat "; " (List.map value_expr values))
  | Map entries ->
      Printf.sprintf "c.map [ %s ]"
        (String.concat "; "
           (List.map
              (fun (key, value) ->
                Printf.sprintf "(%s, %s)" (value_expr key) (value_expr value))
              entries))
  | Set values ->
      Printf.sprintf "c.set [ %s ]"
        (String.concat "; " (List.map value_expr values))
  | List values ->
      Printf.sprintf "c.list_ [ %s ]"
        (String.concat "; " (List.map value_expr values))
  | Tagged (tag, value) ->
      Printf.sprintf "c.tagged (%s, %s)" (escape_ocaml_string tag)
        (value_expr value)

let () =
  let output = Sys.argv.(1) in
  let random = Random.State.make [| 0x5472616e; 0x736974 |] in
  let values = Gen.generate ~rand:random ~n:64 (value 3) in
  let channel = open_out output in
  output_string channel "type 'value constructors = {\n";
  output_string channel "  null : 'value;\n";
  output_string channel "  bool : bool -> 'value;\n";
  output_string channel "  string : string -> 'value;\n";
  output_string channel "  int : int -> 'value;\n";
  output_string channel "  int64 : int64 -> 'value;\n";
  output_string channel "  float : float -> 'value;\n";
  output_string channel "  binary : string -> 'value;\n";
  output_string channel "  keyword : string -> 'value;\n";
  output_string channel "  symbol : string -> 'value;\n";
  output_string channel "  big_decimal : string -> 'value;\n";
  output_string channel "  big_int : string -> 'value;\n";
  output_string channel "  date : int64 -> 'value;\n";
  output_string channel "  uuid : string -> 'value;\n";
  output_string channel "  uri : string -> 'value;\n";
  output_string channel "  array : 'value list -> 'value;\n";
  output_string channel "  map : ('value * 'value) list -> 'value;\n";
  output_string channel "  set : 'value list -> 'value;\n";
  output_string channel "  list_ : 'value list -> 'value;\n";
  output_string channel "  tagged : string * 'value -> 'value;\n";
  output_string channel "}\n\n";
  output_string channel "let values c =\n";
  output_string channel "  [\n";
  List.iter
    (fun value ->
      Printf.fprintf channel "    %s;\n" (value_expr value))
    values;
  output_string channel "  ]\n";
  close_out channel

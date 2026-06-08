module Json = Transit.Json
module Edn = Melange_edn
open Json

let write ?mode value = Json.to_string ?mode value
let read text = Json.of_string text

let check_string expected actual =
  ignore Jest.Expect.(expect actual |> toEqual expected)

let check_value expected actual =
  ignore Jest.Expect.(expect actual |> toEqual expected)
let json_string value = Js.Json.stringify value
let parse_json text = Js.Json.parseExn text

let check_float expected = function
  | Float actual when Float.equal expected actual -> ()
  | actual ->
      failwith
        (Printf.sprintf "expected float %.17g, got %s" expected
           (write actual))

let any value = Edn.any value
let edn_nil = any Edn.nil
let edn_bool value = any (Edn.bool value)
let edn_string value = any (Edn.string value)
let edn_char value = any (Edn.char value)
let edn_keyword value = any (Edn.keyword value)
let edn_int value = any (Edn.int value)
let edn_bigint value = any (Edn.bigint value)
let edn_decimal value = any (Edn.decimal value)
let edn_list values = any (Edn.list values)
let edn_vector values = any (Edn.vector values)
let edn_set values = any (Edn.set values)
let edn_map entries = any (Edn.map entries)
let edn_tagged tag value = any (Edn.tagged tag value)

let () =
  Jest.describe "Transit JSON writer" (fun () ->
      Jest.test "writes ground scalars" (fun () ->
          check_string "null" (write Null);
          check_string "true" (write (Bool true));
          check_string "false" (write (Bool false));
          check_string "42" (write (Int 42));
          check_string "\"~i9007199254740992\""
            (write (Int64 9_007_199_254_740_992L));
          check_string "1.25" (write (Float 1.25));
          check_string "\"hello\"" (write (String "hello"));
          check_string "\"~~x\"" (write (String "~x"));
          check_string "\"~^x\"" (write (String "^x"));
          check_string "\"~`x\"" (write (String "`x"));
          Jest.pass);
      Jest.test "writes maps and caches keys in normal mode" (fun () ->
          check_string
            "[\"^ \",\"~_\",\"nil\",\"~?t\",\"yes\",\"~i7\",\"seven\",\"~d1.5\",\"one-five\",\"name\",\"Ada\"]"
            (write
               (Map
                  [
                    (Null, String "nil");
                    (Bool true, String "yes");
                    (Int 7, String "seven");
                    (Float 1.5, String "one-five");
                    (String "name", String "Ada");
                  ]));
          check_string "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]"
            (write
               (Array
                  [
                    Keyword "color";
                    Keyword "color";
                    Symbol "thing";
                    Symbol "thing";
                  ]));
          check_string "[\"^ \",\"name\",\"Ada\",\"^0\",\"Grace\"]"
            (write
               (Map
                  [
                    (String "name", String "Ada");
                    (String "name", String "Grace");
                  ]));
          Jest.pass);
      Jest.test "writes extension and composite values" (fun () ->
          check_string "\"~baGk=\"" (write (Bytes "hi"));
          check_string "\"~f123.456\"" (write (Big_decimal "123.456"));
          check_string "\"~n12345678901234567890\""
            (write (Big_int "12345678901234567890"));
          check_string "\"~m123456789\"" (write (Time 123_456_789L));
          check_string "\"~u531a379e-31bb-4ce1-8690-158dceb64be6\""
            (write (Uuid "531a379e-31bb-4ce1-8690-158dceb64be6"));
          check_string "\"~rhttps://example.com\""
            (write (Uri "https://example.com"));
          check_string "\"~cx\"" (write (Char "x"));
          check_string "\"~zNaN\"" (write (Float Float.nan));
          check_string "\"~zINF\"" (write (Float infinity));
          check_string "\"~z-INF\"" (write (Float neg_infinity));
          check_string "[1,\"two\",true]"
            (write (Array [ Int 1; String "two"; Bool true ]));
          check_string "[\"~#set\",[\"a\",\"b\"]]"
            (write (Set [ String "a"; String "b" ]));
          check_string "[\"~#list\",[\"a\"]]" (write (List [ String "a" ]));
          check_string "[\"~#'\",\"literal\"]"
            (write (Quote (String "literal")));
          check_string "[\"~#point\",[10,20]]"
            (write (Tagged ("point", Array [ Int 10; Int 20 ])));
          check_string "[\"~#cmap\",[[1,2],\"point\"]]"
            (write (Map [ (Array [ Int 1; Int 2 ], String "point") ]));
          Jest.pass);
      Jest.test "writes verbose mode without cache" (fun () ->
          check_string "{\"name\":\"Ada\",\"~:role\":\"dev\"}"
            (write ~mode:Verbose
               (Map
                  [
                    (String "name", String "Ada");
                    (Keyword "role", String "dev");
                  ]));
          check_string "[\"~:color\",\"~:color\"]"
            (write ~mode:Verbose (Array [ Keyword "color"; Keyword "color" ]));
          Jest.pass);
      Jest.test "exposes Js.Json values" (fun () ->
          check_string "[\"^ \",\"name\",\"Ada\"]"
            (json_string (Json.to_json (Map [ (String "name", String "Ada") ])));
          Jest.pass))

let () =
  Jest.describe "Transit JSON reader" (fun () ->
      Jest.test "reads ground scalars" (fun () ->
          check_value Null (read "null");
          check_value (Bool true) (read "true");
          check_value (Bool false) (read "false");
          check_value (Int 42) (read "42");
          check_value (Int64 9_007_199_254_740_992L)
            (read "\"~i9007199254740992\"");
          check_float 1.25 (read "1.25");
          check_value (String "hello") (read "\"hello\"");
          check_value (String "~x") (read "\"~~x\"");
          check_value (String "^x") (read "\"~^x\"");
          check_value (String "`x") (read "\"~`x\"");
          Jest.pass);
      Jest.test "reads string tags" (fun () ->
          check_value Null (read "\"~_\"");
          check_value (Bool true) (read "\"~?t\"");
          check_value (Bool false) (read "\"~?f\"");
          check_value (Int 7) (read "\"~i7\"");
          check_float 1.5 (read "\"~d1.5\"");
          check_value (Bytes "hi") (read "\"~baGk=\"");
          check_value (Keyword "color") (read "\"~:color\"");
          check_value (Symbol "thing") (read "\"~$thing\"");
          check_value (Big_decimal "123.456") (read "\"~f123.456\"");
          check_value (Big_int "12345678901234567890")
            (read "\"~n12345678901234567890\"");
          check_value (Time 123_456_789L) (read "\"~m123456789\"");
          check_value (Uuid "531a379e-31bb-4ce1-8690-158dceb64be6")
            (read "\"~u531a379e-31bb-4ce1-8690-158dceb64be6\"");
          check_value (Uri "https://example.com")
            (read "\"~rhttps://example.com\"");
          check_value (Char "x") (read "\"~cx\"");
          check_float infinity (read "\"~zINF\"");
          check_float neg_infinity (read "\"~z-INF\"");
          Jest.pass);
      Jest.test "reads maps, cache entries, and composites" (fun () ->
          check_value
            (Map
               [
                 (Null, String "nil");
                 (Bool true, String "yes");
                 (Int 7, String "seven");
                 (Float 1.5, String "one-five");
                 (String "name", String "Ada");
               ])
            (read
               "[\"^ \",\"~_\",\"nil\",\"~?t\",\"yes\",\"~i7\",\"seven\",\"~d1.5\",\"one-five\",\"name\",\"Ada\"]");
          check_value
            (Map
               [
                 (String "name", String "Ada");
                 (Keyword "role", String "dev");
               ])
            (read "{\"name\":\"Ada\",\"~:role\":\"dev\"}");
          check_value
            (Array
               [ Keyword "color"; Keyword "color"; Symbol "thing"; Symbol "thing" ])
            (read "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]");
          check_value
            (Map
               [
                 (String "name", String "Ada");
                 (String "name", String "Grace");
               ])
            (read "[\"^ \",\"name\",\"Ada\",\"^0\",\"Grace\"]");
          check_value (Array [ Int 1; String "two"; Bool true ])
            (read "[1,\"two\",true]");
          check_value (Set [ String "a"; String "b" ])
            (read "[\"~#set\",[\"a\",\"b\"]]");
          check_value (List [ String "a" ]) (read "[\"~#list\",[\"a\"]]");
          check_value (Quote (String "literal")) (read "[\"~#'\",\"literal\"]");
          check_value (Tagged ("point", Array [ Int 10; Int 20 ]))
            (read "[\"~#point\",[10,20]]");
          check_value (Map [ (Array [ Int 1; Int 2 ], String "point") ])
            (read "[\"~#cmap\",[[1,2],\"point\"]]");
          check_value (Tagged ("point", Array [ Int 10; Int 20 ]))
            (read "{\"~#point\":[10,20]}");
          Jest.pass);
      Jest.test "reads Js.Json values" (fun () ->
          check_value (Array [ Int 1; String "two"; Bool true ])
            (Json.of_json (parse_json "[1,\"two\",true]"));
          Jest.pass))

let () =
  Jest.describe "Transit EDN conversion" (fun () ->
      Jest.test "converts native EDN values" (fun () ->
          let edn =
            edn_map
              [
                (edn_keyword "name", edn_string "Ada");
                (edn_keyword "active", edn_bool true);
                (edn_keyword "none", edn_nil);
                (edn_keyword "score", edn_int 42L);
                (edn_keyword "large", edn_int 9_007_199_254_740_992L);
                (edn_keyword "decimal", edn_decimal "1.20");
                (edn_keyword "bigint", edn_bigint "12345678901234567890");
                (edn_keyword "roles", edn_vector [ edn_string "admin"; edn_string "dev" ]);
                (edn_keyword "items", edn_list [ edn_int 1L; edn_int 2L ]);
                (edn_keyword "flags", edn_set [ edn_keyword "fast"; edn_keyword "safe" ]);
              ]
          in
          let transit =
            Map
              [
                (Keyword "name", String "Ada");
                (Keyword "active", Bool true);
                (Keyword "none", Null);
                (Keyword "score", Int 42);
                (Keyword "large", Int64 9_007_199_254_740_992L);
                (Keyword "decimal", Big_decimal "1.20");
                (Keyword "bigint", Big_int "12345678901234567890");
                (Keyword "roles", Array [ String "admin"; String "dev" ]);
                (Keyword "items", List [ Int 1; Int 2 ]);
                (Keyword "flags", Set [ Keyword "fast"; Keyword "safe" ]);
              ]
          in
          check_value edn (Json.to_edn transit);
          check_value transit (Json.of_edn edn);
          Jest.pass);
      Jest.test "converts extension EDN values" (fun () ->
          let edn =
            edn_vector
              [
                edn_char (Uchar.of_char 'x');
                edn_tagged "transit/bytes" (edn_string "hi");
                edn_tagged "transit/time" (edn_int 123_456_789L);
                edn_tagged "uuid"
                  (edn_string "531a379e-31bb-4ce1-8690-158dceb64be6");
                edn_tagged "transit/uri" (edn_string "https://example.com");
                edn_tagged "transit/quote" (edn_string "literal");
                edn_tagged "point" (edn_vector [ edn_int 10L; edn_int 20L ]);
              ]
          in
          let transit =
            Array
              [
                Char "x";
                Bytes "hi";
                Time 123_456_789L;
                Uuid "531a379e-31bb-4ce1-8690-158dceb64be6";
                Uri "https://example.com";
                Quote (String "literal");
                Tagged ("point", Array [ Int 10; Int 20 ]);
              ]
          in
          check_value edn (Json.to_edn transit);
          check_value transit (Json.of_edn edn);
          Jest.pass))

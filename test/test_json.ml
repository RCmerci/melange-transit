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
        (Printf.sprintf "expected float %.17g, got %s" expected (write actual))

let check_int64 expected = function
  | Int64 actual when Int64.equal expected actual -> ()
  | actual ->
      failwith
        (Printf.sprintf "expected int64 %s, got %s" (Int64.to_string expected)
           (write actual))

let check_int expected = function
  | Int actual when actual = expected -> ()
  | actual ->
      failwith
        (Printf.sprintf "expected int %d, got %s" expected (write actual))

let any value = Edn.any value
let edn_nil = any Edn.nil
let edn_bool value = any (Edn.bool value)
let edn_string value = any (Edn.string value)
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
  Jest.describe "Transit JSON transit-js binding" (fun () ->
      Jest.test "writes using transit-js reader/writer semantics" (fun () ->
          check_string "[\"~#'\",null]" (write Null);
          check_string "[\"~#'\",true]" (write (Bool true));
          check_string "[\"~#'\",42]" (write (Int 42));
          check_string "[\"~#'\",1.25]" (write (Float 1.25));
          check_string "[\"~#'\",\"hello\"]" (write (String "hello"));
          check_string "[\"~#'\",\"~:color\"]" (write (Keyword "color"));
          check_string "[\"~#'\",\"~$thing\"]" (write (Symbol "thing"));
          check_string "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]"
            (write
               (Array
                  [
                    Keyword "color";
                    Keyword "color";
                    Symbol "thing";
                    Symbol "thing";
                  ]));
          check_string "[\"^ \",\"name\",\"Grace\"]"
            (write
               (Map
                  [
                    (String "name", String "Ada");
                    (String "name", String "Grace");
                  ]));
          check_string "[\"~m123456789\"]" (write (Array [ Date 123_456_789L ]));
          check_string
            "[\"~n12345678901234567890\",\"~f123.456\",\"~u531a379e-31bb-4ce1-8690-158dceb64be6\",\"~rhttps://example.com\"]"
            (write
               (Array
                  [
                    Big_int "12345678901234567890";
                    Big_decimal "123.456";
                    Uuid "531a379e-31bb-4ce1-8690-158dceb64be6";
                    Uri "https://example.com";
                  ]));
          check_string "[\"~#set\",[\"a\",\"b\"]]"
            (write (Set [ String "a"; String "b" ]));
          check_string "[\"~#list\",[\"a\"]]" (write (List [ String "a" ]));
          check_string "[\"~#point\",[10,20]]"
            (write (Tagged ("point", Array [ Int 10; Int 20 ])));
          check_string "[\"~#cmap\",[[1,2],\"point\"]]"
            (write (Map [ (Array [ Int 1; Int 2 ], String "point") ]));
          Jest.pass);
      Jest.test "passes verbose mode through to transit-js" (fun () ->
          check_string "{\"name\":\"Grace\"}"
            (write ~mode:Verbose
               (Map
                  [
                    (String "name", String "Ada");
                    (String "name", String "Grace");
                  ]));
          check_string "[\"~:color\",\"~:color\"]"
            (write ~mode:Verbose (Array [ Keyword "color"; Keyword "color" ]));
          check_string "[\"~t1970-01-02T10:17:36.789Z\"]"
            (write ~mode:Verbose (Array [ Date 123_456_789L ]));
          Jest.pass);
      Jest.test "reads values produced by transit-js" (fun () ->
          check_value Null (read "[\"~#'\",null]");
          check_value (Bool true) (read "[\"~#'\",true]");
          check_value (Int 42) (read "[\"~#'\",42]");
          check_int 2_147_483_647 (read "[\"~#'\",2147483647]");
          check_int (-2_147_483_648) (read "[\"~#'\",-2147483648]");
          check_int64 2_147_483_648L (read "[\"~#'\",2147483648]");
          check_int64 (-2_147_483_649L) (read "[\"~#'\",-2147483649]");
          check_int64 9_007_199_254_740_992L
            (read "[\"~#'\",9007199254740992]");
          check_int64 1_777_063_533_827L
            (read "{\"~:block/created-at\":1777063533827}"
            |> function
            | Map [ (Keyword "block/created-at", value) ] -> value
            | value -> value);
          check_float 1.25 (read "[\"~#'\",1.25]");
          check_float 1.5
            (read "{\"~:block/score\":1.5}"
            |> function
            | Map [ (Keyword "block/score", value) ] -> value
            | value -> value);
          check_value (String "hello") (read "[\"~#'\",\"hello\"]");
          check_value (Keyword "color") (read "[\"~#'\",\"~:color\"]");
          check_value (Symbol "thing") (read "[\"~#'\",\"~$thing\"]");
          check_value
            (Array
               [ Keyword "color"; Keyword "color"; Symbol "thing"; Symbol "thing" ])
            (read "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]");
          check_value (Map [ (String "name", String "Grace") ])
            (read "[\"^ \",\"name\",\"Grace\"]");
          check_value (Array [ Date 123_456_789L ])
            (read "[\"~t1970-01-02T10:17:36.789Z\"]");
          check_value
            (Array
               [
                 Big_int "12345678901234567890";
                 Big_decimal "123.456";
                 Uuid "531a379e-31bb-4ce1-8690-158dceb64be6";
                 Uri "https://example.com";
               ])
            (read
               "[\"~n12345678901234567890\",\"~f123.456\",\"~u531a379e-31bb-4ce1-8690-158dceb64be6\",\"~rhttps://example.com\"]");
          check_value (Set [ String "a"; String "b" ])
            (read "[\"~#set\",[\"a\",\"b\"]]");
          check_value (List [ String "a" ]) (read "[\"~#list\",[\"a\"]]");
          check_value (Tagged ("point", Array [ Int 10; Int 20 ]))
            (read "[\"~#point\",[10,20]]");
          check_value (Map [ (Array [ Int 1; Int 2 ], String "point") ])
            (read "[\"~#cmap\",[[1,2],\"point\"]]");
          check_value (String "x") (read "\"~cx\"");
          check_value (String "literal") (read "[\"~#'\",\"literal\"]");
          Jest.pass);
      Jest.test "exposes Js.Json values via the transit-js writer" (fun () ->
          check_string "[\"^ \",\"name\",\"Ada\"]"
            (json_string (Json.to_json (Map [ (String "name", String "Ada") ])));
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
      Jest.test "converts EDN keywords parsed by melange-edn" (fun () ->
          check_value (Keyword "my.ns/name")
            (Json.of_edn (Edn.of_edn_string ":my.ns/name"));
          Jest.pass);
      Jest.test "converts EDN values added in melange-edn master" (fun () ->
          check_value (Tagged ("edn/ratio", String "22/7"))
            (Json.of_edn (Edn.of_edn_string "22/7"));
          check_value (Tagged ("edn/regex", String "[a-z]+"))
            (Json.of_edn (Edn.of_edn_string "#\"[a-z]+\""));
          check_value (edn_tagged "edn/ratio" (edn_string "22/7"))
            (Json.to_edn (Tagged ("edn/ratio", String "22/7")));
          check_value (edn_tagged "edn/regex" (edn_string "[a-z]+"))
            (Json.to_edn (Tagged ("edn/regex", String "[a-z]+")));
          Jest.pass);
      Jest.test "converts extension EDN values" (fun () ->
          let edn =
            edn_vector
              [
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
                Binary "hi";
                Date 123_456_789L;
                Uuid "531a379e-31bb-4ce1-8690-158dceb64be6";
                Uri "https://example.com";
                Tagged ("transit/quote", String "literal");
                Tagged ("point", Array [ Int 10; Int 20 ]);
              ]
          in
          check_value edn (Json.to_edn transit);
          check_value transit (Json.of_edn edn);
          Jest.pass))

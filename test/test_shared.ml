module type Json = sig
  type mode = Transit_common.Transit_types.Json.mode =
    | Normal
    | Verbose

  type value = Transit_common.Transit_types.Json.value =
    | Null
    | Bool of bool
    | String of string
    | Int of int
    | Int64 of int64
    | Float of float
    | Binary of string
    | Keyword of string
    | Symbol of string
    | Big_decimal of string
    | Big_int of string
    | Date of int64
    | Uuid of string
    | Uri of string
    | Array of value list
    | Map of (value * value) list
    | Set of value list
    | List of value list
    | Tagged of string * value

  exception Decode_error of string

  val to_string : ?mode:mode -> value -> string
  val of_string : string -> value
end

module Make (Json : Json) = struct
  open Json

  let fail message = failwith message

  let check_string name expected actual =
    if not (String.equal expected actual) then
      fail
        (Printf.sprintf "%s: expected %S, got %S" name expected actual)

  let check_value name expected actual =
    if not (expected = actual) then
      fail
        (Printf.sprintf "%s: decoded value did not match expected value" name)

  let check_float name expected = function
    | Float actual when Float.equal expected actual -> ()
    | _ -> fail (Printf.sprintf "%s: expected float %.17g" name expected)

  let check_int64 name expected = function
    | Int64 actual when Int64.equal expected actual -> ()
    | _ -> fail (Printf.sprintf "%s: expected int64 %Ld" name expected)

  let check_int name expected = function
    | Int actual when actual = expected -> ()
    | _ -> fail (Printf.sprintf "%s: expected int %d" name expected)

  let fixed_write_cases =
    [
      ("write-null", Null, "[\"~#'\",null]");
      ("write-bool", Bool true, "[\"~#'\",true]");
      ("write-int", Int 42, "[\"~#'\",42]");
      ("write-float", Float 1.25, "[\"~#'\",1.25]");
      ("write-string", String "hello", "[\"~#'\",\"hello\"]");
      ("write-escaped-string", Array [ String "~x"; String "^x"; String "`x" ],
       "[\"~~x\",\"~^x\",\"~`x\"]");
      ("write-keyword", Keyword "color", "[\"~#'\",\"~:color\"]");
      ("write-symbol", Symbol "thing", "[\"~#'\",\"~$thing\"]");
      ("write-cache",
       Array [ Keyword "color"; Keyword "color"; Symbol "thing"; Symbol "thing" ],
       "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]");
      ("write-map-duplicate-key",
       Map [ (String "name", String "Ada"); (String "name", String "Grace") ],
       "[\"^ \",\"name\",\"Grace\"]");
      ("write-date", Array [ Date 123_456_789L ], "[\"~m123456789\"]");
      ("write-int64-safe", Array [ Int64 2_147_483_648L ], "[2147483648]");
      ("write-int64-unsafe", Array [ Int64 9_007_199_254_740_992L ],
       "[\"~i9007199254740992\"]");
      ("write-special-scalars",
       Array
         [
           Big_int "12345678901234567890";
           Big_decimal "123.456";
           Uuid "531a379e-31bb-4ce1-8690-158dceb64be6";
           Uri "https://example.com";
         ],
       "[\"~n12345678901234567890\",\"~f123.456\",\"~u531a379e-31bb-4ce1-8690-158dceb64be6\",\"~rhttps://example.com\"]");
      ("write-binary", Array [ Binary "hi" ], "[\"~baGk=\"]");
      ("write-set", Set [ String "a"; String "b" ], "[\"~#set\",[\"a\",\"b\"]]");
      ("write-list", List [ String "a" ], "[\"~#list\",[\"a\"]]");
      ("write-tagged", Tagged ("point", Array [ Int 10; Int 20 ]),
       "[\"~#point\",[10,20]]");
      ("write-complex-map", Map [ (Array [ Int 1; Int 2 ], String "point") ],
       "[\"~#cmap\",[[1,2],\"point\"]]");
    ]

  let fixed_verbose_cases =
    [
      ("verbose-map",
       Map [ (String "name", String "Ada"); (String "name", String "Grace") ],
       "{\"name\":\"Grace\"}");
      ("verbose-no-cache", Array [ Keyword "color"; Keyword "color" ],
       "[\"~:color\",\"~:color\"]");
      ("verbose-date", Array [ Date 123_456_789L ],
       "[\"~t1970-01-02T10:17:36.789Z\"]");
    ]

  let fixed_read_cases =
    [
      ("read-null", "[\"~#'\",null]", Null);
      ("read-bool", "[\"~#'\",true]", Bool true);
      ("read-int", "[\"~#'\",42]", Int 42);
      ("read-string", "[\"~#'\",\"hello\"]", String "hello");
      ("read-escaped-string", "\"~~x\"", String "~x");
      ("read-keyword", "[\"~#'\",\"~:color\"]", Keyword "color");
      ("read-symbol", "[\"~#'\",\"~$thing\"]", Symbol "thing");
      ("read-cache", "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]",
       Array [ Keyword "color"; Keyword "color"; Symbol "thing"; Symbol "thing" ]);
      ("read-map", "[\"^ \",\"name\",\"Grace\"]",
       Map [ (String "name", String "Grace") ]);
      ("read-date", "[\"~t1970-01-02T10:17:36.789Z\"]",
       Array [ Date 123_456_789L ]);
      ("read-int64-tag", "[\"~i9007199254740992\"]",
       Array [ Int64 9_007_199_254_740_992L ]);
      ("read-special-scalars",
       "[\"~n12345678901234567890\",\"~f123.456\",\"~u531a379e-31bb-4ce1-8690-158dceb64be6\",\"~rhttps://example.com\"]",
       Array
         [
           Big_int "12345678901234567890";
           Big_decimal "123.456";
           Uuid "531a379e-31bb-4ce1-8690-158dceb64be6";
           Uri "https://example.com";
         ]);
      ("read-binary", "[\"~baGk=\"]", Array [ Binary "hi" ]);
      ("read-set", "[\"~#set\",[\"a\",\"b\"]]",
       Set [ String "a"; String "b" ]);
      ("read-list", "[\"~#list\",[\"a\"]]", List [ String "a" ]);
      ("read-tagged", "[\"~#point\",[10,20]]",
       Tagged ("point", Array [ Int 10; Int 20 ]));
      ("read-complex-map", "[\"~#cmap\",[[1,2],\"point\"]]",
       Map [ (Array [ Int 1; Int 2 ], String "point") ]);
      ("read-quote-ground", "\"~cx\"", String "x");
    ]

  let run_fixed () =
    List.iter
      (fun (name, value, expected) ->
        check_string name expected (to_string value))
      fixed_write_cases;
    List.iter
      (fun (name, value, expected) ->
        check_string name expected (to_string ~mode:Verbose value))
      fixed_verbose_cases;
    List.iter
      (fun (name, text, expected) -> check_value name expected (of_string text))
      fixed_read_cases;
    check_int "read-int-max" 2_147_483_647 (of_string "[\"~#'\",2147483647]");
    check_int "read-int-min" (-2_147_483_648)
      (of_string "[\"~#'\",-2147483648]");
    check_int64 "read-int64-above-int" 2_147_483_648L
      (of_string "[\"~#'\",2147483648]");
    check_int64 "read-int64-below-int" (-2_147_483_649L)
      (of_string "[\"~#'\",-2147483649]");
    check_float "read-float" 1.25 (of_string "[\"~#'\",1.25]")

  let escaped text = String.escaped text

  let run_random_output _platform_name =
    List.iteri
      (fun index value ->
        let normal = to_string value in
        let verbose = to_string ~mode:Verbose value in
        let decode_random mode text =
          try of_string text with
          | Decode_error message ->
              fail
                (Printf.sprintf "random-%s-decode-%d failed for %S: %s" mode
                   index text message)
        in
        let normal_roundtrip = decode_random "normal" normal in
        let verbose_roundtrip = decode_random "verbose" verbose in
        check_value
          (Printf.sprintf "random-normal-roundtrip-%d" index)
          value normal_roundtrip;
        check_value
          (Printf.sprintf "random-verbose-roundtrip-%d" index)
          value verbose_roundtrip;
        Printf.printf "%03d\t%s\t%s\n" index (escaped normal) (escaped verbose))
      Random_cases.values
end

module Json = struct
  type mode =
    | Normal
    | Verbose

  type value =
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

  let decode_error message = raise (Decode_error message)

  let protect_decode f =
    try f () with
    | Decode_error _ as exn -> raise exn
    | exn -> decode_error (Printexc.to_string exn)

  let min_transit_int = -2_147_483_648L
  let max_transit_int = 2_147_483_647L

  let transit_int text =
    match Int64.of_string_opt text with
    | Some value
      when Int64.compare value min_transit_int >= 0
           && Int64.compare value max_transit_int <= 0 ->
        Int (Int64.to_int value)
    | Some value -> Int64 value
    | None -> decode_error ("invalid Transit integer: " ^ text)

  let array_to_list render values =
    Array.fold_right (fun value acc -> render value :: acc) values []

  let edn_any value = Melange_edn.any value
  let edn_string value = edn_any (Melange_edn.string value)
  let edn_int value = edn_any (Melange_edn.int value)
  let edn_vector values = edn_any (Melange_edn.vector values)
  let edn_list values = edn_any (Melange_edn.list values)
  let edn_set values = edn_any (Melange_edn.set values)
  let edn_map entries = edn_any (Melange_edn.map entries)
  let edn_tagged tag value = edn_any (Melange_edn.tagged tag value)

  let string_of_uchar uchar =
    let buffer = Buffer.create 4 in
    Buffer.add_utf_8_uchar buffer uchar;
    Buffer.contents buffer

  let rec to_edn = function
    | Null -> edn_any Melange_edn.nil
    | Bool value -> edn_any (Melange_edn.bool value)
    | String text -> edn_string text
    | Int value -> edn_int (Int64.of_int value)
    | Int64 value -> edn_int value
    | Float value -> edn_any (Melange_edn.float value)
    | Binary text -> edn_tagged "transit/bytes" (edn_string text)
    | Keyword text -> edn_any (Melange_edn.keyword text)
    | Symbol text -> edn_any (Melange_edn.symbol text)
    | Big_decimal text -> edn_any (Melange_edn.decimal text)
    | Big_int text -> edn_any (Melange_edn.bigint text)
    | Date milliseconds -> edn_tagged "transit/time" (edn_int milliseconds)
    | Uuid text -> edn_tagged "uuid" (edn_string text)
    | Uri text -> edn_tagged "transit/uri" (edn_string text)
    | Array values -> edn_vector (List.map to_edn values)
    | Map entries ->
        edn_map (List.map (fun (key, value) -> (to_edn key, to_edn value)) entries)
    | Set values -> edn_set (List.map to_edn values)
    | List values -> edn_list (List.map to_edn values)
    | Tagged (tag, value) -> edn_tagged tag (to_edn value)

  let int64_of_edn tag = function
    | Melange_edn.Any (Melange_edn.Int value) -> value
    | value ->
        decode_error
          (Printf.sprintf "%s tag expects an integer, got %s" tag
             (Melange_edn.to_edn_string value))

  let string_of_edn tag = function
    | Melange_edn.Any (Melange_edn.String value) -> value
    | value ->
        decode_error
          (Printf.sprintf "%s tag expects a string, got %s" tag
             (Melange_edn.to_edn_string value))

  let rec of_edn (Melange_edn.Any value) =
    match value with
    | Melange_edn.Nil -> Null
    | Melange_edn.Bool value -> Bool value
    | Melange_edn.String text -> String text
    | Melange_edn.Char uchar -> String (string_of_uchar uchar)
    | Melange_edn.Symbol text -> Symbol text
    | Melange_edn.Keyword keyword ->
        Keyword (Melange_edn.keyword_to_string keyword)
    | Melange_edn.Int value -> transit_int (Int64.to_string value)
    | Melange_edn.Bigint text -> Big_int text
    | Melange_edn.Float value -> Float value
    | Melange_edn.Decimal text -> Big_decimal text
    | Melange_edn.Ratio text -> Tagged ("edn/ratio", String text)
    | Melange_edn.Regex pattern -> Tagged ("edn/regex", String pattern)
    | Melange_edn.List values -> List (array_to_list of_edn values)
    | Melange_edn.Vector values -> Array (array_to_list of_edn values)
    | Melange_edn.Map entries ->
        Map
          (array_to_list
             (fun (key, value) -> (of_edn key, of_edn value))
             entries)
    | Melange_edn.Set values -> Set (array_to_list of_edn values)
    | Melange_edn.Tagged ("transit/bytes", value) ->
        Binary (string_of_edn "transit/bytes" value)
    | Melange_edn.Tagged ("transit/time", value) ->
        Date (int64_of_edn "transit/time" value)
    | Melange_edn.Tagged ("uuid", value) -> Uuid (string_of_edn "uuid" value)
    | Melange_edn.Tagged ("transit/uri", value) ->
        Uri (string_of_edn "transit/uri" value)
    | Melange_edn.Tagged (tag, value) -> Tagged (tag, of_edn value)
end

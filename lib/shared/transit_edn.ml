module Json = Transit_core.Json

open Json
open Json.Internal

module type Edn = Melange_edn.S

module Make (Edn : Edn) = struct
  let array_to_list render values =
    Array.fold_right (fun value acc -> render value :: acc) values []

  let edn_any value = Edn.any value
  let edn_string value = edn_any (Edn.string value)
  let edn_int value = edn_any (Edn.int value)
  let edn_vector values = edn_any (Edn.vector values)
  let edn_list values = edn_any (Edn.list values)
  let edn_set values = edn_any (Edn.set values)
  let edn_map entries = edn_any (Edn.map entries)
  let edn_tagged tag value = edn_any (Edn.tagged tag value)

  let string_of_uchar uchar =
    let buffer = Buffer.create 4 in
    Buffer.add_utf_8_uchar buffer uchar;
    Buffer.contents buffer

  let rec to_edn = function
    | Null -> edn_any Edn.nil
    | Bool value -> edn_any (Edn.bool value)
    | String text -> edn_string text
    | Int value -> edn_int (Int64.of_int value)
    | Int64 value -> edn_int value
    | Float value -> edn_any (Edn.float value)
    | Binary text -> edn_tagged "transit/bytes" (edn_string text)
    | Keyword text -> edn_any (Edn.keyword text)
    | Symbol text -> edn_any (Edn.symbol text)
    | Big_decimal text -> edn_any (Edn.decimal text)
    | Big_int text -> edn_any (Edn.bigint text)
    | Date milliseconds -> edn_tagged "transit/time" (edn_int milliseconds)
    | Uuid text -> edn_tagged "uuid" (edn_string text)
    | Uri text -> edn_tagged "transit/uri" (edn_string text)
    | Array values -> edn_vector (List.map to_edn values)
    | Map entries ->
        edn_map
          (List.map (fun (key, value) -> (to_edn key, to_edn value)) entries)
    | Set values -> edn_set (List.map to_edn values)
    | List values -> edn_list (List.map to_edn values)
    | Tagged (tag, value) -> edn_tagged tag (to_edn value)

  let int64_of_edn tag = function
    | Edn.Any (Edn.Int value) -> value
    | value ->
        decode_error
          (Printf.sprintf "%s tag expects an integer, got %s" tag
             (Edn.to_edn_string value))

  let string_of_edn tag = function
    | Edn.Any (Edn.String value) -> value
    | value ->
        decode_error
          (Printf.sprintf "%s tag expects a string, got %s" tag
             (Edn.to_edn_string value))

  let rec of_edn (Edn.Any value) =
    match value with
    | Edn.Nil -> Null
    | Edn.Bool value -> Bool value
    | Edn.String text -> String text
    | Edn.Char uchar -> String (string_of_uchar uchar)
    | Edn.Symbol text -> Symbol text
    | Edn.Keyword keyword -> Keyword (Edn.keyword_to_string keyword)
    | Edn.Int value -> transit_int (Int64.to_string value)
    | Edn.Bigint text -> Big_int text
    | Edn.Float value -> Float value
    | Edn.Decimal text -> Big_decimal text
    | Edn.Ratio text -> Tagged ("edn/ratio", String text)
    | Edn.Regex pattern -> Tagged ("edn/regex", String pattern)
    | Edn.List values -> List (array_to_list of_edn values)
    | Edn.Vector values -> Array (array_to_list of_edn values)
    | Edn.Map entries ->
        Map
          (array_to_list
             (fun (key, value) -> (of_edn key, of_edn value))
             entries)
    | Edn.Set values -> Set (array_to_list of_edn values)
    | Edn.Tagged ("transit/bytes", value) ->
        Binary (string_of_edn "transit/bytes" value)
    | Edn.Tagged ("transit/time", value) ->
        Date (int64_of_edn "transit/time" value)
    | Edn.Tagged ("uuid", value) -> Uuid (string_of_edn "uuid" value)
    | Edn.Tagged ("transit/uri", value) -> Uri (string_of_edn "transit/uri" value)
    | Edn.Tagged (tag, value) -> Tagged (tag, of_edn value)
end

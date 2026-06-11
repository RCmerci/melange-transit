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

  module Transit_js = struct
    type t
    type reader
    type writer
    type reader_options

    external reader_options : preferBuffers:bool -> unit -> reader_options = ""
    [@@mel.obj]

    external reader : string -> reader_options -> reader = "reader"
    [@@mel.module "transit-js"]

    external writer : string -> writer = "writer" [@@mel.module "transit-js"]
    external read : reader -> string -> t = "read" [@@mel.send]
    external write : writer -> t -> string = "write" [@@mel.send]
    external date : string -> t = "date" [@@mel.module "transit-js"]
    external integer : string -> t = "integer" [@@mel.module "transit-js"]
    external uuid : string -> t = "uuid" [@@mel.module "transit-js"]
    external big_int : string -> t = "bigInt" [@@mel.module "transit-js"]
    external big_decimal : string -> t = "bigDec" [@@mel.module "transit-js"]
    external keyword : string -> t = "keyword" [@@mel.module "transit-js"]
    external symbol : string -> t = "symbol" [@@mel.module "transit-js"]
    external binary : string -> t = "binary" [@@mel.module "transit-js"]
    external uri : string -> t = "uri" [@@mel.module "transit-js"]
    external map : t array -> t = "map" [@@mel.module "transit-js"]
    external set : t array -> t = "set" [@@mel.module "transit-js"]
    external list : t array -> t = "list" [@@mel.module "transit-js"]
    external tagged : string -> t -> t = "tagged" [@@mel.module "transit-js"]
    external is_integer : t -> bool = "isInteger" [@@mel.module "transit-js"]
    external is_uuid : t -> bool = "isUUID" [@@mel.module "transit-js"]
    external is_big_int : t -> bool = "isBigInt" [@@mel.module "transit-js"]
    external is_big_decimal : t -> bool = "isBigDec" [@@mel.module "transit-js"]
    external is_keyword : t -> bool = "isKeyword" [@@mel.module "transit-js"]
    external is_symbol : t -> bool = "isSymbol" [@@mel.module "transit-js"]
    external is_binary : t -> bool = "isBinary" [@@mel.module "transit-js"]
    external is_uri : t -> bool = "isURI" [@@mel.module "transit-js"]
    external is_map : t -> bool = "isMap" [@@mel.module "transit-js"]
    external is_set : t -> bool = "isSet" [@@mel.module "transit-js"]
    external is_list : t -> bool = "isList" [@@mel.module "transit-js"]
    external is_tagged_value : t -> bool = "isTaggedValue"
    [@@mel.module "transit-js"]

    external to_string : t -> string = "toString" [@@mel.send]
    external tag : t -> string = "tag" [@@mel.get]
    external rep : t -> t = "rep" [@@mel.get]
    external for_each_map : t -> ((t -> t -> unit)[@mel.uncurry]) -> unit = "forEach"
    [@@mel.send]

    external for_each_set : t -> ((t -> t -> unit)[@mel.uncurry]) -> unit = "forEach"
    [@@mel.send]

    external get_time_method : t -> (unit -> float) Js.undefined = "getTime"
    [@@mel.get]

    external get_time : t -> float = "getTime" [@@mel.send]
    external length : t -> int = "length" [@@mel.get]
    external get_index : t -> int -> int = "" [@@mel.get_index]
  end

  let decode_error message = raise (Decode_error message)

  let protect_decode f =
    try f () with
    | Decode_error _ as exn -> raise exn
    | exn -> decode_error (Printexc.to_string exn)

  let mode_type = function
    | Normal -> "json"
    | Verbose -> "json-verbose"

  let base64_encode text =
    let alphabet =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    in
    let len = String.length text in
    let output = Buffer.create (((len + 2) / 3) * 4) in
    let add index = Buffer.add_char output alphabet.[index] in
    let rec loop offset =
      if offset < len then (
        let b0 = Char.code text.[offset] in
        let has_b1 = offset + 1 < len in
        let has_b2 = offset + 2 < len in
        let b1 = if has_b1 then Char.code text.[offset + 1] else 0 in
        let b2 = if has_b2 then Char.code text.[offset + 2] else 0 in
        add (b0 lsr 2);
        add (((b0 land 0x03) lsl 4) lor (b1 lsr 4));
        if has_b1 then add (((b1 land 0x0f) lsl 2) lor (b2 lsr 6))
        else Buffer.add_char output '=';
        if has_b2 then add (b2 land 0x3f) else Buffer.add_char output '=';
        loop (offset + 3))
    in
    loop 0;
    Buffer.contents output

  let uint8_array_to_string bytes =
    let output = Buffer.create (Transit_js.length bytes) in
    for index = 0 to Transit_js.length bytes - 1 do
      Buffer.add_char output (Char.chr (Transit_js.get_index bytes index))
    done;
    Buffer.contents output

  let js_null = Obj.magic Js.null
  let js_bool value = Obj.magic (Js.Json.boolean value)
  let js_number value = Obj.magic (Js.Json.number value)
  let js_string value = Obj.magic (Js.Json.string value)
  let js_array values = Obj.magic values

  let rec to_js = function
    | Null -> js_null
    | Bool value -> js_bool value
    | String text -> js_string text
    | Int value -> js_number (Float.of_int value)
    | Int64 value -> Transit_js.integer (Int64.to_string value)
    | Float value -> js_number value
    | Binary text -> Transit_js.binary (base64_encode text)
    | Keyword text -> Transit_js.keyword text
    | Symbol text -> Transit_js.symbol text
    | Big_decimal text -> Transit_js.big_decimal text
    | Big_int text -> Transit_js.big_int text
    | Date milliseconds -> Transit_js.date (Int64.to_string milliseconds)
    | Uuid text -> Transit_js.uuid text
    | Uri text -> Transit_js.uri text
    | Array values -> values |> List.map to_js |> Array.of_list |> js_array
    | Map entries ->
        entries
        |> List.concat_map (fun (key, value) -> [ to_js key; to_js value ])
        |> Array.of_list |> Transit_js.map
    | Set values -> values |> List.map to_js |> Array.of_list |> Transit_js.set
    | List values -> values |> List.map to_js |> Array.of_list |> Transit_js.list
    | Tagged (tag, value) -> Transit_js.tagged tag (to_js value)

  let transit_int text =
    match int_of_string_opt text with
    | Some value -> Int value
    | None -> (
        match Int64.of_string_opt text with
        | Some value -> Int64 value
        | None -> decode_error ("invalid Transit integer: " ^ text))

  let number_value value =
    if value = floor value then
      match int_of_string_opt (Js.Float.toString value) with
      | Some int -> Int int
      | None -> Float value
    else Float value

  let keyword_name value =
    let text = Transit_js.to_string value in
    if String.length text > 0 && text.[0] = ':' then
      String.sub text 1 (String.length text - 1)
    else text

  let tagged_rep_text value = Transit_js.rep value |> Transit_js.to_string

  let is_date value =
    match Js.undefinedToOption (Transit_js.get_time_method value) with
    | Some _ -> true
    | None -> false

  let rec of_js value =
    match Js.Json.classify (Obj.magic value) with
    | JSONNull -> Null
    | JSONFalse -> Bool false
    | JSONTrue -> Bool true
    | JSONString text -> String text
    | JSONNumber number -> number_value number
    | JSONArray values ->
        Array (values |> Array.to_list |> List.map (fun value -> of_js (Obj.magic value)))
    | JSONObject object_ ->
        if Transit_js.is_keyword value then Keyword (keyword_name value)
        else if Transit_js.is_symbol value then Symbol (Transit_js.to_string value)
        else if Transit_js.is_uuid value then Uuid (Transit_js.to_string value)
        else if Transit_js.is_big_int value then Big_int (tagged_rep_text value)
        else if Transit_js.is_big_decimal value then Big_decimal (tagged_rep_text value)
        else if Transit_js.is_uri value then Uri (tagged_rep_text value)
        else if Transit_js.is_binary value then Binary (uint8_array_to_string value)
        else if Transit_js.is_map value then Map (map_entries value)
        else if Transit_js.is_set value then Set (set_values value)
        else if Transit_js.is_list value then List (tagged_array_rep value)
        else if Transit_js.is_tagged_value value then
          Tagged (Transit_js.tag value, of_js (Transit_js.rep value))
        else if is_date value then
          Date (Int64.of_float (Transit_js.get_time value))
        else if Transit_js.is_integer value then
          transit_int (Transit_js.to_string value)
        else
          Map
            (object_ |> Js.Dict.entries |> Array.to_list
            |> List.map (fun (key, value) -> (String key, of_js (Obj.magic value))))

  and map_entries value =
    let entries = ref [] in
    Transit_js.for_each_map value
      (fun value key -> entries := (of_js key, of_js value) :: !entries);
    List.rev !entries

  and set_values value =
    let values = ref [] in
    Transit_js.for_each_set value (fun value _ -> values := of_js value :: !values);
    List.rev !values

  and tagged_array_rep value =
    match Js.Json.classify (Obj.magic (Transit_js.rep value)) with
    | JSONArray values ->
        values |> Array.to_list |> List.map (fun value -> of_js (Obj.magic value))
    | _ -> decode_error ("Transit list expects an array representation")

  let to_string ?(mode = Normal) value =
    Transit_js.writer (mode_type mode) |> fun writer ->
    Transit_js.write writer (to_js value)

  let to_json ?mode value = value |> to_string ?mode |> Js.Json.parseExn

  let of_string text =
    protect_decode (fun () ->
        let reader =
          Transit_js.reader "json" (Transit_js.reader_options ~preferBuffers:false ())
        in
        Transit_js.read reader text |> of_js)

  let of_json json = json |> Js.Json.stringify |> of_string

  let iarray_to_list render values =
    Iarray.fold_right (fun value acc -> render value :: acc) values []

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
    | Melange_edn.List values -> List (iarray_to_list of_edn values)
    | Melange_edn.Vector values -> Array (iarray_to_list of_edn values)
    | Melange_edn.Map entries ->
        Map
          (iarray_to_list
             (fun (key, value) -> (of_edn key, of_edn value))
             entries)
    | Melange_edn.Set values -> Set (iarray_to_list of_edn values)
    | Melange_edn.Tagged ("transit/bytes", value) ->
        Binary (string_of_edn "transit/bytes" value)
    | Melange_edn.Tagged ("transit/time", value) ->
        Date (int64_of_edn "transit/time" value)
    | Melange_edn.Tagged ("uuid", value) -> Uuid (string_of_edn "uuid" value)
    | Melange_edn.Tagged ("transit/uri", value) ->
        Uri (string_of_edn "transit/uri" value)
    | Melange_edn.Tagged (tag, value) -> Tagged (tag, of_edn value)
end

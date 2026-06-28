module Json = struct
  include Transit_core.Json
  open Internal

  type edn = Melange_edn_melange.any

  module Edn = Transit_edn.Make (Melange_edn_melange)

  let to_edn = Edn.to_edn
  let of_edn = Edn.of_edn

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

  let mode_type = function
    | Normal -> "json"
    | Verbose -> "json-verbose"

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

  let min_melange_int_number = Float.of_int min_int
  let max_melange_int_number = Float.of_int max_int

  let number_value value =
    number_value ~min_int_number:min_melange_int_number
      ~max_int_number:max_melange_int_number value

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
        Array
          (values |> Array.to_list |> List.map (fun value -> of_js (Obj.magic value)))
    | JSONObject object_ ->
        if Transit_js.is_keyword value then
          Keyword (keyword_name (Transit_js.to_string value))
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
        else if is_date value then Date (Int64.of_float (Transit_js.get_time value))
        else if Transit_js.is_integer value then transit_int (Transit_js.to_string value)
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
    | _ -> decode_error "Transit list expects an array representation"

  let to_string ?(mode = Normal) value =
    Transit_js.writer (mode_type mode) |> fun writer ->
    Transit_js.write writer (to_js value)

  let of_string text =
    protect_decode (fun () ->
        let reader =
          Transit_js.reader "json" (Transit_js.reader_options ~preferBuffers:false ())
        in
        Transit_js.read reader text |> of_js)
end

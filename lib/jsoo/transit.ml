open Js_of_ocaml

module Json = struct
  include Transit_common.Transit_types.Json

  module Unsafe = Js.Unsafe

  let js_string text = Unsafe.inject (Js.string text)
  let js_bool value = Unsafe.inject (Js.bool value)
  let js_number value = Unsafe.inject (Js.number_of_float value)
  let js_null () : Unsafe.any = Unsafe.js_expr "null"
  let js_require () : Unsafe.any = Unsafe.js_expr "require('transit-js')"
  let js_typeof () : Unsafe.any = Unsafe.js_expr "(function(value){return typeof value;})"
  let js_is_null () : Unsafe.any = Unsafe.js_expr "(function(value){return value === null;})"
  let js_is_array () : Unsafe.any = Unsafe.js_expr "Array.isArray"
  let js_length () : Unsafe.any = Unsafe.js_expr "(function(value){return value.length;})"
  let js_get_index () : Unsafe.any = Unsafe.js_expr "(function(value,index){return value[index];})"
  let js_object_keys () : Unsafe.any = Unsafe.js_expr "Object.keys"

  let js_map_entries () : Unsafe.any =
    Unsafe.js_expr
      "(function(value){var entries=[]; value.forEach(function(v,k){entries.push(k); entries.push(v);}); return entries;})"

  let js_set_values () : Unsafe.any =
    Unsafe.js_expr
      "(function(value){var entries=[]; value.forEach(function(v){entries.push(v);}); return entries;})"

  let fun_call fn args = Unsafe.fun_call fn args
  let meth_call obj name args = Unsafe.meth_call obj name args
  let call name args = meth_call (js_require ()) name args
  let as_string value = Js.to_string (Obj.magic value)
  let as_bool value = Js.to_bool (Obj.magic value)
  let as_float value = Js.float_of_number (Obj.magic value)
  let as_int value = int_of_float (as_float value)
  let typeof value = as_string (fun_call (js_typeof ()) [| value |])
  let is_null value = as_bool (fun_call (js_is_null ()) [| value |])
  let is_array value = as_bool (fun_call (js_is_array ()) [| value |])
  let length value = as_int (fun_call (js_length ()) [| value |])
  let get_index value index =
    fun_call (js_get_index ()) [| value; js_number (Float.of_int index) |]

  let array_of_list values =
    values |> Array.of_list |> Js.array |> Unsafe.inject

  let reader_options () =
    Unsafe.obj [| ("preferBuffers", js_bool false) |]

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
    let output = Buffer.create (length bytes) in
    for index = 0 to length bytes - 1 do
      Buffer.add_char output (Char.chr (as_int (get_index bytes index)))
    done;
    Buffer.contents output

  let rec to_js = function
    | Null -> js_null ()
    | Bool value -> js_bool value
    | String text -> js_string text
    | Int value -> js_number (Float.of_int value)
    | Int64 value -> call "integer" [| js_string (Int64.to_string value) |]
    | Float value -> js_number value
    | Binary text -> call "binary" [| js_string (base64_encode text) |]
    | Keyword text -> call "keyword" [| js_string text |]
    | Symbol text -> call "symbol" [| js_string text |]
    | Big_decimal text -> call "bigDec" [| js_string text |]
    | Big_int text -> call "bigInt" [| js_string text |]
    | Date milliseconds -> call "date" [| js_string (Int64.to_string milliseconds) |]
    | Uuid text -> call "uuid" [| js_string text |]
    | Uri text -> call "uri" [| js_string text |]
    | Array values -> values |> List.map to_js |> array_of_list
    | Map entries ->
        entries
        |> List.concat_map (fun (key, value) -> [ to_js key; to_js value ])
        |> array_of_list |> fun entries -> call "map" [| entries |]
    | Set values ->
        values |> List.map to_js |> array_of_list |> fun values ->
        call "set" [| values |]
    | List values ->
        values |> List.map to_js |> array_of_list |> fun values ->
        call "list" [| values |]
    | Tagged (tag, value) -> call "tagged" [| js_string tag; to_js value |]

  let min_jsoo_int_number = Float.of_int min_int
  let max_jsoo_int_number = Float.of_int max_int
  let min_int64_number = Int64.to_float Int64.min_int
  let max_int64_number = Int64.to_float Int64.max_int

  let int64_of_exact_number value =
    if value >= min_int64_number && value <= max_int64_number then
      let int64 = Int64.of_float value in
      if Float.equal (Int64.to_float int64) value then Some int64 else None
    else None

  let number_value value =
    match classify_float value with
    | FP_normal | FP_subnormal | FP_zero when Float.equal value (floor value) ->
        if value >= min_jsoo_int_number && value <= max_jsoo_int_number then
          Int (int_of_float value)
        else (
          match int64_of_exact_number value with
          | Some int64 -> Int64 int64
          | None -> Float value)
    | FP_normal | FP_subnormal | FP_zero | FP_nan | FP_infinite -> Float value

  let transit_is name value = as_bool (call name [| value |])
  let to_js_string value = as_string (meth_call value "toString" [||])
  let get_prop value name = Unsafe.get value name
  let tag value = as_string (get_prop value "tag")
  let rep value = get_prop value "rep"

  let keyword_name value =
    let text = to_js_string value in
    if String.length text > 0 && Char.equal text.[0] ':' then
      String.sub text 1 (String.length text - 1)
    else text

  let tagged_rep_text value = rep value |> to_js_string

  let is_date value =
    String.equal (typeof (get_prop value "getTime")) "function"

  let date_time value = as_float (meth_call value "getTime" [||])

  let rec of_js value =
    if is_null value then Null
    else
      match typeof value with
      | "boolean" -> Bool (as_bool value)
      | "number" -> number_value (as_float value)
      | "string" -> String (as_string value)
      | "object" ->
          if is_array value then array_value value
          else if transit_is "isKeyword" value then Keyword (keyword_name value)
          else if transit_is "isSymbol" value then Symbol (to_js_string value)
          else if transit_is "isUUID" value then Uuid (to_js_string value)
          else if transit_is "isBigInt" value then Big_int (tagged_rep_text value)
          else if transit_is "isBigDec" value then Big_decimal (tagged_rep_text value)
          else if transit_is "isURI" value then Uri (tagged_rep_text value)
          else if transit_is "isBinary" value then Binary (uint8_array_to_string value)
          else if transit_is "isMap" value then Map (map_entries value)
          else if transit_is "isSet" value then Set (set_values value)
          else if transit_is "isList" value then List (tagged_array_rep value)
          else if transit_is "isTaggedValue" value then Tagged (tag value, of_js (rep value))
          else if is_date value then Date (Int64.of_float (date_time value))
          else if transit_is "isInteger" value then transit_int (to_js_string value)
          else object_entries value
      | kind -> decode_error ("unsupported JavaScript value kind: " ^ kind)

  and array_value value =
    let values = ref [] in
    for index = 0 to length value - 1 do
      values := of_js (get_index value index) :: !values
    done;
    Array (List.rev !values)

  and map_entries value =
    let entries = fun_call (js_map_entries ()) [| value |] in
    let result = ref [] in
    let rec loop index =
      if index < length entries then (
        result :=
          (of_js (get_index entries index), of_js (get_index entries (index + 1)))
          :: !result;
        loop (index + 2))
    in
    loop 0;
    List.rev !result

  and set_values value =
    let entries = fun_call (js_set_values ()) [| value |] in
    let result = ref [] in
    for index = 0 to length entries - 1 do
      result := of_js (get_index entries index) :: !result
    done;
    List.rev !result

  and tagged_array_rep value =
    let values = rep value in
    if not (is_array values) then decode_error "Transit list expects an array representation";
    let result = ref [] in
    for index = 0 to length values - 1 do
      result := of_js (get_index values index) :: !result
    done;
    List.rev !result

  and object_entries value =
    let keys = fun_call (js_object_keys ()) [| value |] in
    let entries = ref [] in
    for index = 0 to length keys - 1 do
      let key = as_string (get_index keys index) in
      entries := (String key, of_js (Unsafe.get value key)) :: !entries
    done;
    Map (List.rev !entries)

  let to_string ?(mode = Normal) value =
    let writer = call "writer" [| js_string (mode_type mode) |] in
    as_string (meth_call writer "write" [| to_js value |])

  let of_string text =
    protect_decode (fun () ->
        let reader = call "reader" [| js_string "json"; reader_options () |] in
        meth_call reader "read" [| js_string text |] |> of_js)
end

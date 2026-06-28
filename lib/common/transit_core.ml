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

  module Internal = struct
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

    let min_int64_number = Int64.to_float Int64.min_int
    let max_int64_number = Int64.to_float Int64.max_int

    let int64_of_exact_number value =
      if value >= min_int64_number && value <= max_int64_number then
        let int64 = Int64.of_float value in
        if Float.equal (Int64.to_float int64) value then Some int64 else None
      else None

    let number_value ~min_int_number ~max_int_number value =
      match classify_float value with
      | FP_normal | FP_subnormal | FP_zero when Float.equal value (floor value) ->
          if value >= min_int_number && value <= max_int_number then
            Int (int_of_float value)
          else (
            match int64_of_exact_number value with
            | Some int64 -> Int64 int64
            | None -> Float value)
      | FP_normal | FP_subnormal | FP_zero | FP_nan | FP_infinite -> Float value

    let keyword_name text =
      if String.length text > 0 && Char.equal text.[0] ':' then
        String.sub text 1 (String.length text - 1)
      else text
  end

end

module type Backend = sig
  type edn

  type mode = Json.mode =
    | Normal
    | Verbose

  type value = Json.value =
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
  val to_edn : value -> edn
  val of_string : string -> value
  val of_edn : edn -> value
end

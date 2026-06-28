module Json : sig
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

  module Internal : sig
    val decode_error : string -> 'a
    val protect_decode : (unit -> 'a) -> 'a
    val transit_int : string -> value
    val base64_encode : string -> string
    val number_value :
      min_int_number:float -> max_int_number:float -> float -> value
    val keyword_name : string -> string
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

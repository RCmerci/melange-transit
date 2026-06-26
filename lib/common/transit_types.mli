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

  val decode_error : string -> 'a
  val protect_decode : (unit -> 'a) -> 'a
  val transit_int : string -> value
  val to_edn : value -> Melange_edn.any
  val of_edn : Melange_edn.any -> value
end

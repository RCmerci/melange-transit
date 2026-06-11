module Json : sig
  (** Transit JSON write mode. [Normal] enables Transit caching and writes maps
      with the ["^ "] map-as-array marker. [Verbose] disables caching and writes
      stringable maps as JSON objects. *)
  type mode =
    | Normal
    | Verbose

  (** Transit values represented by transit-js runtime kinds. *)
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

  (** Encode a Transit value as a JavaScript JSON value. *)
  val to_json : ?mode:mode -> value -> Js.Json.t

  (** Encode a Transit value as a JSON string. *)
  val to_string : ?mode:mode -> value -> string

  (** Convert a Transit value to an EDN value. Transit values without a native
      EDN representation are encoded as tagged EDN values. *)
  val to_edn : value -> Melange_edn.any

  (** Decode a Transit value from a JavaScript JSON value. *)
  val of_json : Js.Json.t -> value

  (** Decode a Transit value from a JSON string. *)
  val of_string : string -> value

  (** Convert an EDN value to a Transit value. Recognized tagged EDN values are
      decoded back to their Transit-specific representations. *)
  val of_edn : Melange_edn.any -> value
end

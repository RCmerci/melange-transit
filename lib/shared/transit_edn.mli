module type Edn = Melange_edn.S

module Make (Edn : Edn) : sig
  val to_edn : Transit_core.Json.value -> Edn.any
  val of_edn : Edn.any -> Transit_core.Json.value
end

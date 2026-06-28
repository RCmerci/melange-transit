# melange-transit

Transit JSON reader and writer for native OCaml, js_of_ocaml, and Melange.

The js_of_ocaml and Melange backends wrap the
[cognitect/transit-js](https://github.com/cognitect/transit-js) JSON reader and
writer. The native OCaml backend implements the same Transit JSON behavior in
OCaml. MessagePack is intentionally out of scope because `transit-js` does not
support MessagePack encoding.

The `Transit.Json.value` constructors mirror the transit-js runtime kinds that
can be represented consistently: `Binary` maps to transit-js binary values, and
`Date` maps to JavaScript `Date` values stored as milliseconds since the Unix
epoch. Transit char and quote ground values are exposed as their underlying
values because the transit-js reader unwraps them.

## Usage

Use the platform library that matches your target:

- `melange-transit-core` for shared Transit JSON types and EDN conversion
- `melange-transit-native` for native OCaml
- `melange-transit-jsoo` for js_of_ocaml
- `melange-transit-melange` for Melange

The platform backends depend on `melange-transit-core` and expose the same
`Transit.Json` value API under their wrapped library module:

```ocaml
module Json = Transit_native.Transit.Json

let payload =
  Json.Map
    [
      (Json.Keyword "name", Json.String "Ada");
      (Json.Keyword "roles", Json.Array [ Json.String "admin" ]);
    ]

let json = Json.to_string payload
let decoded = Json.of_string json
```

Normal JSON mode uses `transit.writer("json")`. Verbose mode uses
`transit.writer("json-verbose")`:

```ocaml
let json = Json.to_string ~mode:Json.Verbose payload
```

`transit-js` runtime semantics are preserved. For example, top-level scalar
values are quote-wrapped by the writer, duplicate map keys follow JavaScript Map
semantics, and `json-verbose` date values are written as ISO timestamps.

## Development

```sh
dune build @runtest @test/cross-runtest
```

`@runtest` runs the fixed and QCheck-generated cases for each backend.
`@test/cross-runtest` diffs the encoded output across native, js_of_ocaml, and
Melange.

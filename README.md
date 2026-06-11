# melange-transit

Transit JSON reader and writer bindings for Melange.

This library wraps the
[cognitect/transit-js](https://github.com/cognitect/transit-js) JSON reader and
writer while exposing OCaml values through the existing `Transit.Json` API.
MessagePack is intentionally out of scope because `transit-js` does not support
MessagePack encoding.

The `Transit.Json.value` constructors mirror the transit-js runtime kinds that
can be represented consistently: `Binary` maps to transit-js binary values, and
`Date` maps to JavaScript `Date` values stored as milliseconds since the Unix
epoch. Transit char and quote ground values are exposed as their underlying
values because the transit-js reader unwraps them.

## Usage

```ocaml
let payload =
  Transit.Json.Map
    [
      (Transit.Json.Keyword "name", Transit.Json.String "Ada");
      (Transit.Json.Keyword "roles", Transit.Json.Array [ Transit.Json.String "admin" ]);
    ]

let json = Transit.Json.to_string payload
let decoded = Transit.Json.of_string json
```

Normal JSON mode uses `transit.writer("json")`. Verbose mode uses
`transit.writer("json-verbose")`:

```ocaml
let json = Transit.Json.to_string ~mode:Transit.Json.Verbose payload
```

`transit-js` runtime semantics are preserved. For example, top-level scalar
values are quote-wrapped by the writer, duplicate map keys follow JavaScript Map
semantics, and `json-verbose` date values are written as ISO timestamps.

## Development

```sh
dune runtest
```

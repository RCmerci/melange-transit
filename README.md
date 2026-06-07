# melange-transit

Transit JSON reader and writer for Melange.

This library implements JSON string encoding and decoding for the
[Transit 0.8 format](https://github.com/cognitect/transit-format). MessagePack
is intentionally out of scope for now.

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

Normal JSON mode writes maps as Transit map arrays and enables Transit caching.
Verbose mode writes stringable maps as JSON objects and disables caching:

```ocaml
let json = Transit.Json.to_string ~mode:Transit.Json.Verbose payload
```

## Development

```sh
dune runtest
```

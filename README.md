# ocaml-corvus-json-schema

A [Bowtie](https://github.com/bowtie-json-schema/bowtie) test harness for the OCaml package corvus-json-schema, part of
[Corvus.JsonSchema](https://github.com/corvus-dotnet/Corvus.JsonSchema): a port of the Corvus runtime evaluator to OCaml,
with no dependencies outside the OCaml distribution
([corvus-json-schema-ocaml](https://github.com/corvus-dotnet/corvus-json-schema-ocaml)).

Its image is published to `ghcr.io/bowtie-json-schema/ocaml-corvus-json-schema` and run via
`bowtie run -i ocaml-corvus-json-schema`.

The image builds the harness (`bowtie_corvus_json_schema.ml`) with dune, together with the package's released source
from `ghcr.io/corvus-dotnet/corvus-json-schema-ocaml`, on Alpine's OCaml. The harness reads Bowtie's requests with
yojson's `Yojson.Raw`, so schemas and instances reach the package as Bowtie wrote them, numbers included. Dependabot
keeps the image tag in the Dockerfile up to date.

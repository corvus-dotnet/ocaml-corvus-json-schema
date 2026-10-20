# The package's source, as released (src-ml/corvus-json-schema in Corvus.JsonSchema, published to
# corvus-dotnet/corvus-json-schema-ocaml). Its version is this tag, which Dependabot keeps up to date.
FROM ghcr.io/corvus-dotnet/corvus-json-schema-ocaml:0.1.1 AS corvus

FROM alpine:3.24 AS build
RUN apk add --no-cache ocaml ocaml-compiler-libs dune ocaml-yojson-dev musl-dev
COPY --from=corvus /corvus-json-schema /src/corvus-json-schema
COPY dune-project dune bowtie_corvus_json_schema.ml /src/
# The package's own tests and tools are not built.
RUN rm -rf /src/corvus-json-schema/test /src/corvus-json-schema/tools \
    && cd /src && dune build --profile release ./bowtie_corvus_json_schema.exe

FROM alpine:3.24
COPY --from=build /src/_build/default/bowtie_corvus_json_schema.exe /usr/local/bin/bowtie_corvus_json_schema
CMD ["bowtie_corvus_json_schema"]

# Tech stack

## OCaml and libraries

All tools and shared libraries are written in OCaml. Prefer Jane Street libraries, such as `Base` or `Core`, where they fit the code being written.

## Build and package management

[opam](https://opam.ocaml.org/) manages the OCaml compiler and package dependencies. [Dune](https://dune.build/) builds the tools and runs their tests.

Declare package dependencies in [`dune-project`](../dune-project) and link libraries in the relevant `dune` stanzas. Dune generates the package manifests in [`opam/`](../opam/) from `dune-project`.

See the [README](../README.md) for the commands to install dependencies and build from source.

## Coding conventions

Follow Anil Madhavapeddy’s OCaml development conventions where they fit this repository and the code you’re changing. [OCaml development skills](https://github.com/avsm/ocaml-claude-marketplace/tree/main/plugins/ocaml-dev/skills)

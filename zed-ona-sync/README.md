# zed-ona-sync

A tool for syncing Zed editor settings.

## Prerequisites

- OCaml (5.0+)
- opam
- Dune

## Building

Install dependencies:

```bash
opam install . --deps-only
```

Build the project:

```bash
dune build
```

## Running

```bash
dune exec zed-ona-sync
```

Or run the compiled binary directly:

```bash
./_build/default/bin/main.exe
```


# ona-env-port-forward

Port forwarding tool for Ona environments with auto-reconnect and connection tracking.

## Usag

```bash
ona-env-port-forward [port]   # default: 5173
```

## Development

Install dependencies:

```bash
opam install . --deps-only
```

Build the project:

```bash
dune build
```

Build & run

```bash
dune exec ona-env-port-forward
```

Installing into `~/.opam/default/bin`

```bash
dune install
```
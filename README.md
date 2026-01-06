# Tools

Tools meant just for me. Written in OCaml, mostly by LLMs.

## Installation

Build and install all executables to `/usr/local/bin`:

```bash
opam install . --deps-only
dune build
sudo dune install --prefix=/usr/local
```

This installs the binaries system-wide, making them available without activating the OPAM switch.

## Available Tools

- `tool-ona-env-port-forward` - Port forwarding for Ona environments with auto-reconnect
- `tool-ona-log-msg-parser` - Extract timestamp and msg from ona-swe-agent-service logs
- `tool-ona-sync-zed-ssh-connections` - Sync Zed ssh_connections settings with Ona environments
# ona-env-port-forward

Port forwarding tool for Gitpod (Ona) environments with auto-reconnect and connection tracking.

## Installation

```bash
# Install dependencies (note: CI=false is needed due to a dune-configurator bug)
CI=false opam install minttea spices yojson

# Build and install
dune build
dune install
```

## Dependencies

- OCaml >= 5.1
- yojson >= 2.0
- minttea >= 0.0.2 (TUI framework)
- spices >= 0.0.2 (styling)
- gitpod CLI

## Usage

```bash
ona-env-port-forward [port]   # default: 5173
dune exec ona-env-port-forward
```

## Features

- Lists running Gitpod environments using the gitpod CLI
- Interactive TUI for environment selection (using minttea)
- SSH port forwarding with auto-reconnect on connection loss
- Connection logging with timestamps and duration tracking
- Session summary on exit (total connections, uptime, average connection duration)

## Example Output

```
Select a Gitpod environment:

▸ my-feature-branch (env-abc123def)
  main (env-xyz789ghi)

↑/k up • ↓/j down • enter select • q quit
```

After selection:
```
[2025-12-04 10:30:15] Starting port forwarding to my-feature-branch (port 5173)
[2025-12-04 10:30:15] Connected to my-feature-branch (port 5173)
[2025-12-04 11:45:22] Disconnected after 1h15m7s (exited with code 255, reconnecting... attempt #2)
[2025-12-04 11:45:24] Connected to my-feature-branch (port 5173)
^C
Session summary: 2 connection(s), total uptime 2h30m15s, avg connection 1h15m7s
```

## How it works

1. Queries `gitpod environment list --running-only` for running environments
2. Presents a minttea TUI selector with arrow key/vim navigation
3. Establishes SSH port forwarding with keepalive settings:
   - `ServerAliveInterval=30` - sends keepalive every 30s
   - `ServerAliveCountMax=3` - disconnects after 3 missed keepalives
4. On disconnect, logs duration and automatically reconnects after 2s delay
5. On Ctrl+C, prints session summary with connection statistics

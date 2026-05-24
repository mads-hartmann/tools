# tool-ona-env-port-forward

Port forwarding for Ona environments with auto-reconnect and connection tracking.

Presents an interactive TUI to select a running environment, then SSH-forwards
the chosen port with automatic reconnection on disconnect. Prints a session
summary (connection count, total uptime) on exit.

> **Note:** Only available on macOS. The `minttea` TUI dependency does not
> build on Linux.

## Usage

```
tool-ona-env-port-forward [-p PORT]
```

- `-p`, `--port` — port to forward (default: 5173)

## Examples

```sh
# Forward the default port (5173)
tool-ona-env-port-forward

# Forward a specific port
tool-ona-env-port-forward -p 3000
```

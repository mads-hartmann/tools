# Tools

Tools meant just for me. Written in OCaml, mostly by LLMs.

## Available tools

| Tool | Description | Linux | macOS |
|---|---|---|---|
| [tool-md-serve](bin/tool-md-serve/README.md) | Serve a folder of Markdown files over HTTP | ✅ | ✅ |
| [tool-ona-log-msg-parser](bin/tool-ona-log-msg-parser/README.md) | Extract timestamp and msg from ona-swe-agent-service logs | ✅ | ✅ |
| [tool-ona-sync-zed-ssh-connections](bin/tool-ona-sync-zed-ssh-connections/README.md) | Sync Zed ssh_connections settings with Ona environments | ✅ | ✅ |
| [tool-ona-env-port-forward](bin/tool-ona-env-port-forward/README.md) | Port forwarding for Ona environments with auto-reconnect | ❌ | ✅ |

## Installation

### From a release (recommended)

Download the binary for your platform from the [latest release](https://github.com/mads-hartmann/tools/releases/latest),
make it executable, and move it onto your `PATH`:

```sh
# Example: install tool-md-serve on macOS
curl -Lo tool-md-serve https://github.com/mads-hartmann/tools/releases/latest/download/tool-md-serve-macos
chmod +x tool-md-serve
mv tool-md-serve /usr/local/bin/
```

Replace `macos` with `linux` for Linux builds. Not all tools are available on
both platforms — see the table above.

### From source

Requires [opam](https://opam.ocaml.org/) and OCaml ≥ 5.0.

```sh
opam install ./opam/ona.opam \
             ./opam/tool-md-serve.opam \
             ./opam/tool-ona-log-msg-parser.opam \
             ./opam/tool-ona-sync-zed-ssh-connections.opam \
             --deps-only -y
dune build
sudo dune install --prefix=/usr/local
```

To also build `tool-ona-env-port-forward` (macOS only), add
`./opam/tool-ona-env-port-forward.opam` to the `opam install` command.

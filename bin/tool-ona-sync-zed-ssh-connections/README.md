# tool-ona-sync-zed-ssh-connections

Keeps Zed's `ssh_connections` config in sync with running Ona environments.
Non-Ona SSH connections in the config are preserved.

## Usage

```
tool-ona-sync-zed-ssh-connections [-c PATH] [-i SECONDS] [--once]
```

- `-c`, `--config` — path to `settings.json` (default: `~/.config/zed/settings.json`)
- `-i`, `--interval` — sync interval in seconds when running continuously (default: 30)
- `--once` — sync once and exit instead of looping

## Examples

```sh
# Run continuously, syncing every 30 seconds
tool-ona-sync-zed-ssh-connections

# Sync once and exit
tool-ona-sync-zed-ssh-connections --once

# Sync every 60 seconds
tool-ona-sync-zed-ssh-connections -i 60
```

## Raycast

A Raycast script command is available at
[`raycast-script-commands/ona-sync-zed-ssh-connections.sh`](../../raycast-script-commands/ona-sync-zed-ssh-connections.sh).

# tool-ona-log-msg-parser

Parses systemd journal log lines from `ona-swe-agent-service`. Extracts the
timestamp and message from each line, outputting `timestamp | message`.

## Usage

```
tool-ona-log-msg-parser [FILE]
```

Reads from `FILE` if provided, otherwise reads from stdin.

## Examples

```sh
# Parse a log file
tool-ona-log-msg-parser ona-swe-agent-service.log

# Parse from stdin
journalctl -u ona-swe-agent-service | tool-ona-log-msg-parser
```

## Input format

Expects systemd journal lines with a 15-character `Mon DD HH:MM:SS` timestamp
prefix. Messages are extracted from either:

- JSON log lines — the value of the `"msg"` field
- Plain `INFO` lines — the text after the `INFO` marker

Lines that don't match either format are silently skipped.

# tool-md-serve

Serves a folder of Markdown files over HTTP.

## Usage

```
tool-md-serve [DIR] [-p PORT]
```

- `DIR` — directory to serve (default: current directory)
- `-p`, `--port` — port to listen on (default: 8080)

URL paths map directly to the folder structure. A file at `docs/guide/intro.md`
is served at `/guide/intro`. An `index.md` file is served at the root of its
directory.

## Content negotiation

The response format is chosen from the `Accept` header:

| Accept value | Response |
|---|---|
| `text/html` (default) | Markdown rendered to HTML |
| `text/markdown` | Raw Markdown source |
| `text/plain` | Plain-text rendering (tags stripped) |

## Examples

```sh
# Serve the current directory
tool-md-serve

# Serve ./docs on port 3000
tool-md-serve ./docs -p 3000

# Fetch as plain text
curl -H "Accept: text/plain" http://localhost:8080/readme

# Fetch raw Markdown
curl -H "Accept: text/markdown" http://localhost:8080/readme
```

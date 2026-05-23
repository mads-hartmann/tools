# API Reference

A nested page served at `/reference/api`.

## Endpoints

### `GET /`

Returns the root index page.

### `GET /:path`

Returns the Markdown file at the given path. Use the `Accept` header to
control the response format:

- `text/html` — rendered HTML (default)
- `text/markdown` — raw Markdown source
- `text/plain` — plain-text rendering

# Contributing

## Cutting a release

Releases are triggered by pushing a version tag. The CI will build binaries for
Linux and macOS and attach them to a GitHub release automatically.

1. Make sure all changes are merged to `main` and CI is green.

2. Pick a version number following [semver](https://semver.org/) (e.g. `v1.2.0`).

3. Tag and push:

   ```sh
   git tag v1.2.0
   git push origin v1.2.0
   ```

4. The [CI workflow](.github/workflows/ci.yml) will build the matrix, then create
   a GitHub release at <https://github.com/mads-hartmann/tools/releases> with the
   binaries attached and auto-generated release notes.

> `tool-ona-env-port-forward` is macOS-only and will not appear in the Linux
> release assets — this is expected.

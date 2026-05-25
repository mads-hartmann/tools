---
name: cut-release
description: Cut a release for the mads-hartmann/tools repo. Finds the latest git tag, increments it, then tags and pushes to trigger the CI release workflow. Use when asked to "cut a release", "release", "tag a release", or "publish a release".
---

# Cut a release

## Prerequisites

- Working directory is the repo root
- On the `main` branch with a clean working tree
- All changes intended for the release are merged and CI is green on `main`

## Steps

1. **Confirm the branch and working tree are clean:**

   ```sh
   git checkout main && git pull && git status
   ```

   Stop and tell the user if there are uncommitted changes or if CI is not green.

2. **Determine the next tag** using the script. Default is a patch bump:

   ```sh
   bash .gitpod/skills/cut-release/scripts/next-tag.sh          # patch
   bash .gitpod/skills/cut-release/scripts/next-tag.sh minor     # minor
   bash .gitpod/skills/cut-release/scripts/next-tag.sh major     # major
   ```

   If the user specified a version explicitly, use that instead.

3. **Confirm with the user** — show the tag that will be created and ask them to confirm before proceeding.

4. **Tag and push:**

   ```sh
   git tag <TAG>
   git push origin <TAG>
   ```

5. **Report the outcome** — tell the user the tag that was pushed and link to the Actions run:
   `https://github.com/mads-hartmann/tools/actions`

   The CI will build Linux and macOS binaries and publish a GitHub release at:
   `https://github.com/mads-hartmann/tools/releases`

## Notes

- `tool-ona-env-port-forward` will only appear in macOS release assets — this is expected.
- Do not push the tag until the user confirms.

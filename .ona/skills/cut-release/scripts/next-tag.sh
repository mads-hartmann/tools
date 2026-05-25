#!/usr/bin/env bash
# Prints the next patch version tag based on the latest git tag.
# Usage: ./next-tag.sh [major|minor|patch]  (default: patch)

set -euo pipefail

bump="${1:-patch}"

latest=$(git tag --list 'v*' --sort=-version:refname | head -1)

if [ -z "$latest" ]; then
  echo "v0.1.0"
  exit 0
fi

# Strip leading 'v'
version="${latest#v}"
major=$(echo "$version" | cut -d. -f1)
minor=$(echo "$version" | cut -d. -f2)
patch=$(echo "$version" | cut -d. -f3)

case "$bump" in
  major) major=$((major + 1)); minor=0; patch=0 ;;
  minor) minor=$((minor + 1)); patch=0 ;;
  patch) patch=$((patch + 1)) ;;
  *)
    echo "Unknown bump type: $bump. Use major, minor, or patch." >&2
    exit 1
    ;;
esac

echo "v${major}.${minor}.${patch}"

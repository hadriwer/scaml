#!/bin/bash
# Usage: run.sh <scamlc>
#
# Compiles every tests/run/*.scaml and prints its output, then compiles
# every tests/errors/*.scaml and prints the error it must fail with.
# Each program is compiled in a temporary directory, under the same
# relative path (so the paths in error messages stay the same).

scamlc=$(realpath "$1")
here=$(dirname "$0")
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

for f in "$here"/run/*.scaml; do
  name=run/$(basename "$f")
  echo "=== $name"
  mkdir -p "$tmp/run"
  cp "$f" "$tmp/$name"
  if out=$(cd "$tmp" && "$scamlc" "$name" 2>&1); then
    (cd "$tmp" && timeout 20 "./${name%.scaml}.exe" 2>&1)
  else
    echo "COMPILATION FAILED"
    echo "$out"
  fi
done

for f in "$here"/errors/*.scaml; do
  name=errors/$(basename "$f")
  echo "=== $name"
  mkdir -p "$tmp/errors"
  cp "$f" "$tmp/$name"
  (cd "$tmp" && "$scamlc" "$name" 2>&1)
  echo "exit $?"
done

#!/usr/bin/env bash

set -e

curPwd="$(pwd)"
root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stackyaml="$root/stack.yaml"

>&2 echo "build:..."
stack --stack-yaml "$stackyaml"\
      build

>&2 echo "run:..."
time stack --stack-yaml "$stackyaml" \
      exec -- opossum-lib-exe \
      "$@"

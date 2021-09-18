#!/usr/bin/env bash

# SPDX-FileCopyrightText: Maximilian Huber
# SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>
#
# SPDX-License-Identifier: BSD-3-Clause

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

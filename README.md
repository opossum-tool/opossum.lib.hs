<!--
SPDX-FileCopyrightText: Maximilian Huber
SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>

SPDX-License-Identifier: BSD-3-Clause
-->

# opOSSum-lib

[![REUSE status](https://api.reuse.software/badge/git.fsfe.org/reuse/api)](https://api.reuse.software/info/git.fsfe.org/reuse/api)

This is a helper library for generating inputs for the OpossumUI

* **State:** work in progress / hacky 

## usage of CLI
This project contains the helper script `./opossum-lib-exe.sh`, that can be executed out of the box, and it builds on demand.

``` shell
$ ./opossum-lib-exe.sh --help
 ARG [ARG [ARG ...]]]
where ARG one of
            FILE           <-- parse opossum file
            DIR            <-- generate opossum from file tree
 --spdx     SPDX_JSON      <-- parse .spdx.json
 --spdx     SPDX_YAML      <-- parse .spdx.yaml
 --scancode SCANCODE_JSON  <-- parse scancode json
```

You can run the following command, to generate an input file from several input files, a scancode file and a spdx file.

``` shell
$ ./opossum-lib-exe.sh \
        path/to/input1.json \
        path/to/input2.json.gz \
        --scancode path/to/scancode.json \
        --spdx path/to/some.spdx.json \
        > target/path/to/file.json
```

## Dev
### To build
```
$ stack build
```

### To test
```
$ stack test --file-watch
```

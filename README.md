# opOSSum-lib

## usage of CLI
once it is build (see Dev -> To build) one can run the following commands

### Merge Opossum files
```
$ stack exec opOSSum-lib-exe -- --merge-opossums opossum-input-1.json opossum-input-2.json.gz opossum-input-3.json > opossum-output.json 
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

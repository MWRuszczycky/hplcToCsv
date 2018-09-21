# hplcToCsv

Convert Beckman ascii HPLC data files to csv files.

## Installation

This program uses [Haskell Stack](https://docs.haskellstack.org/en/stable/README/). If you have Stack installed, clone the repository and run the following commands:
```sh
cd hplcToCsv
stack build
```
The executable binary will placed (somewhere) in the `.stack-work` directory. You can fish this out and do with it as you like. Alternatively, you can run the following from the terminal:
```sh
cd hplcToCsv
stack install
```
which will build the executable and install it to the local bin.

## Usage

The basic usage is
```sh
hplcToCsv [FILENAME]..
```
Where `FILENAME` is an ascii data file generated for the HPLC run that you want to convert to a csv file. The files usually have the extension `.dat.asc`. You can list more than one file name, for example
```sh
hplcToCsv fileone.dat.asc filetwo.dat.asc
```
will convert both `fileone.dat.asc` and `filetwo.dat.asc`. If you want to convert all files in the current directory, you can just take advantage of shell expansion and run
```sh
hplcToCsv *
```
The ascii files will not be overwritten and instead new csv files will be generated with the same names and the file extensions replaced with `.csv`. If there is any error in parsing the ascii files, then an error message for the file will be generated and no new file will be generated. **Always check the csv files generated to make sure the conversion is correct**.

You can display usage information and version number using the `-h/--help` option. To display just the version number, use `-v/--version`.

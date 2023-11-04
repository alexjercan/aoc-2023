# Advent of Code 2023

Advent of Code 2023 in Haskell

### Quickstart

To run a day and read the input from stdin use

```
stack run day%%
```

where the format of the day is 2 digits (01, 02, ... 20, 21)

### All Usages

To download the puzzle inputs, copy the `.env.example` file into `.env` and
fill in the session with your cookie from the aoc website. Then you can run

```console
./get
```

to download the necessary input files.

To run the project in watch mode (it will rerun the day that you modify) run

```console
./watcher.sh
````

To build the project you can use

```console
make
```

To run one day in particular you can run

```console
make Day%
```

where `%` is the day number with 2 digits (01, 02, ... 20, 21)


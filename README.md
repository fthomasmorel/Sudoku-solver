# Sudoku-solver
A sudoku solver written in Haskell for Euler Project #96.

Please uncomment line #22 in the main function if you want to get the euler answer

## Data Structure

To manipulate the grid, i define some types :
```
[---|---|---] -> Line  }
[-S-|-S-|-S-]          } => Vector
[---|---|---]          }
----|---|----
[---|---|---] -> Line
[-S-|-S-|-S-]
[---|---|---]
----|---|----
[---|---|---] -> Line
[---|---|---]
[---|---|---]
 ^
 Column

 S as Square : 3x3 Matrix
 Line : a 1x9 List of Int
 Column : a 1x9 List of Int
 Vector : a 1x3 List of Square (for the line of the sudoku)
 Sudoku : a 1x3 List of Vector
```

## Compile & run

Juste run
```sh
$ ghc Sudoku.hs
```

And then
```sh
$ ./Sudoku
```

This will run the Haskell script and solve every sudoku from the ```sudoku.txt``` file.

## Sudoku Format
I used the grid from the Euler Project #96. It is build by following

```
Grid 01
003020600
900305001
001806400
008102900
700000008
006708200
002609500
800203009
005010300
```

The 0 value mean no value since a sudoku just got 1,2..9 value.

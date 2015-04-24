module SudokuConverter

/// maps a sudoku number selection, coordinate, and sudoku square size to a natural number as its column id
let inline mapping (n : int) ((x, y) : int * int) (num : int) =
    //a number in a coordinate has to satisfy, but not the number requirement for its row, its column, and its square
    //such that each square, row and column has all numbers 1 to n^2
    let totalRows = n * n
    let totalColumns = n * n
    let totalSquares = n * n

/// takes an n size for a sudoku and an initial board and returns a matrix and column names
let sudokuToMatrix (n : int) (initial : int[,]) =
    for i = 0 to n * n - 1 do
        for j = 0 to n * n - 1 do
            for k = 1 to n * n do
                
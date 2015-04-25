module SudokuConverter
open BlockUtil

/// maps a sudoku number selection, coordinate, and sudoku square size to the three column ids of the conditions it satisfies, and returns the row
let inline mapping (n : int) ((x, y) : int * int) (num : int) =
    //a number in a coordinate has to satisfy, but not the number requirement for its row, its column, and its square
    //such that each square, row and column has all numbers 1 to n^2
    let offset = n * n * n * n //there are n * n * n * n conditions to satisfy for each row, column and square, because there are n * n of each, and each one needs numbers 1 through n * n in each of them
    let rowValue = y * n * n + (num - 1)
    let colValue = offset + x * n * n + (num - 1)
    let squareValue = 2 * offset + (y / n) * n * n * n + (x / n) * n * n + (num - 1)
    let coord = 3 * offset + (y * n * n) + x
    [rowValue; colValue; squareValue; coord;]

/// Takes a row of three values and returns the x y coordinates and the number that was filled in there
let unMap (n : int) (row : int list) =
    let offset = n * n * n * n
    let y = row.[0] / (n * n)
    let x = (row.[1] - offset) / (n * n)
    let num = row.[0] % (n * n) + 1
    ((x, y), num)

/// Gets the coordinates and numbers of a square in soduko
let numInSquare (n : int) (x, y) num (board : int[,]) =
    let xBase = x / n * n
    let yBase = y / n * n
    let square =
        [
            for i = 0 to n - 1 do
                for j = 0 to n - 1 do
                    yield board.[xBase + i, yBase + j]
        ]
    square |> List.exists ((=) num)

/// takes an n size for a sudoku and an initial board and returns a matrix and column names
let sudokuToMatrix (n : int) (initial : int[,]) =
    let matrix =
        [
            for i = 0 to n * n - 1 do
                for j = 0 to n * n - 1 do
                    for k = 1 to n * n do
                        if i = 3 && j = 1 && k = 2 then
                            let row = initial |> Array2DRow j
                            let column = initial |> Array2DColumn i
                            let square = initial |> numInSquare n (i, j) k
                            ()
                        if initial |> Array2DRow j |> List.forall ((<>) k) // make sure the initial puzzle doesn't already have the number tried in the row
                        && initial |> Array2DColumn i |> List.forall ((<>) k) // don't yield numbers already in the column
                        && not (initial |> numInSquare n (i, j) k) 
                        && initial.[i,j] = 0 then// don't yield choices already in the square
                            yield mapping n (i, j) k
        ]
                
    let columns = [for i in matrix do yield! i] |> Set.ofList |> Set.toList
    (matrix, columns)

let solutionsToSudokus (n : int) initial (solutions : int list list list) =
    [
        for sol in solutions do
            let sudoku = Array2D.copy initial
            for row in sol do
                let ((x, y), num) = unMap n row
                sudoku.[x,y] <- num
            let out = Array2D.zeroCreate (n*n) (n*n)
            for i = 0 to n * n - 1 do
                for j = 0 to n * n - 1 do
                    out.[i,j] <- char (sudoku.[i,j] + 48)
            yield out
    ]
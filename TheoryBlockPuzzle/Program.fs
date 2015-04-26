open Puzzle
open MatrixSolver
open SudokuConverter
open RecursiveSolver
open MatrixPrinter
open BlockUtil
open Window
open InputReader

let bruteForceAndAlsoPrintSolutionStuff (args : string[]) rules =
    let writeums = fileToArray args.[0]
    //printArray 0 writeums
    let (target, blocks) = arrayToPuzzle writeums
    let emptyBoard = Array2D.create (Array2D.length1 target) (Array2D.length2 target) ' '
    let solution = bruteSolve target blocks rules
    System.Console.BackgroundColor <- System.ConsoleColor.Blue
    printPuzzleNode 0 solution
    printfn "%i" (countSolutions target solution)
    System.Console.ReadKey(true) |> ignore

let matrixSolveAndPrint (args : string[]) rules =
    let writeums = fileToArray args.[0]
    let (target, blocks) = arrayToPuzzle writeums
    //(snd puzzle).[2] |> blockToArray |> printArray 0
    let (matrix, columns) = createMatrix (target, blocks) rules
    
    toFileMatrixAsListOfLists matrix
    
    printfn "rows: %i" (List.length matrix)

    let initialNode = {matrix = matrix; matrixColumns = columns; children = []; partialSolution = Some([])}

    let timey = System.Diagnostics.Stopwatch.StartNew ()

    /// run and print successive iterations of algorithm x until it completes
    let rec runX node edges =
        let (nextTree, partialSolution, newEdges) = iterateX node edges
        match partialSolution with
        | Some(sol) ->
//            System.Console.Clear ()
//            printSolution target blocks.Length sol
//            System.Threading.Thread.Sleep 10
            runX nextTree newEdges
        | None ->
            node
    let finalTree = runX initialNode [initialNode]
    System.Console.Clear ()

    for i = 0 to blocks.Length - 1 do
        blocks.[i] |> blockToArray |> printArray 0
        printfn ""

    let solutions = matrixSolutionList rules target blocks (solutionsFromTree finalTree)
    for i in solutions do
        i |> printArray 0
        printfn ""

    timey.Stop ()
    printfn "\nisomorphic solutions: %i solutions: %i, time elapsed: %i, tree size: %i" (countIsoSolutions finalTree) (solutions.Length) timey.ElapsedMilliseconds (countNodes finalTree)
    System.Console.ReadKey(true) |> ignore


let recursiveMatrixSolveAndPrint (args : string[]) rules =
    let writeums = fileToArray args.[0]
    let (target, blocks) = arrayToPuzzle writeums
    //(snd puzzle).[2] |> blockToArray |> printArray 0
    let (matrix, columns) = createMatrix (target, blocks) rules

    toFileMatrixAsListOfLists matrix

    printfn "rows: %i" (List.length matrix)

    let timey = System.Diagnostics.Stopwatch.StartNew ()

    let allSolutions = recursiveSolve matrix columns []

    timey.Stop ()

    for i = 0 to blocks.Length - 1 do
        blocks.[i] |> blockToArray |> printArray 0
        printfn ""

    let solutions = matrixSolutionList rules target blocks allSolutions
    for i in solutions do
        i |> printArray 0
        printfn ""

    let numSolutions =
        let divisor = 
            match identicalBlocks blocks rules with
            | [] -> 1
            | x -> x |> List.map (fun (_, x) -> factorial x) |> List.reduce (fun x y -> x * y)
        solutions.Length / divisor

    printfn "\nisomorphic solutions: %i solutions: %i, time elapsed: %i" (allSolutions.Length) (numSolutions) timey.ElapsedMilliseconds

    System.Console.ReadKey(true) |> ignore

///reads puzzlefiles 0 - 18 in its current directory, then prints metrix and numbers of solutions to a file
let solveAll rules =
    let outFile = new System.IO.StreamWriter("solutionsAndMetrix.txt")
    for i = 0 to 18 do
        let fileName = System.String.Format("puzzlefile{0}.txt", i)
        let writeums = fileToArray fileName
        let (target, blocks) = arrayToPuzzle writeums
        let (matrix, columns) = createMatrix (target, blocks) rules

        printfn "rows: %i" (List.length matrix)

        let timey = System.Diagnostics.Stopwatch.StartNew ()

        let allSolutions = recursiveSolve matrix columns []

        timey.Stop ()

        let solutions = matrixSolutionList rules target blocks allSolutions

        let numSolutions =
            let divisor = identicalBlocks blocks rules |> List.map (fun (_, x) -> factorial x) |> List.reduce (fun x y -> x * y)
            solutions.Length / divisor

        outFile.WriteLine (System.String.Format("\n{3}. isomorphic solutions: {0} solutions: {1}, time elapsed: {2}", (allSolutions.Length), (numSolutions), timey.ElapsedMilliseconds, i))
    outFile.Close ()

///creates a exact cover matrix from a given sudoku and solves it
let solveSudoku () =
    let n = 3
    let initial = 
        [|
            [|8;0;0; 0;0;0; 0;0;0;|];
            [|0;0;3; 6;0;0; 0;0;0;|];
            [|0;7;0; 0;9;0; 2;0;0;|];

            [|0;5;0; 0;0;7; 0;0;0;|];
            [|0;0;0; 0;4;5; 7;0;0;|];
            [|0;0;0; 1;0;0; 0;3;0;|];

            [|0;0;1; 0;0;0; 0;6;8;|];
            [|0;0;8; 5;0;0; 0;1;0;|];
            [|0;9;0; 0;0;0; 4;0;0;|];
        |] |> array2D

    let nsmall = 2
    let smallInitial = 
        [|
            [|0;0; 0;3;|]
            [|2;0; 1;0;|]

            [|0;0; 3;2;|]
            [|3;0; 4;0;|]
        |] |> array2D

    let (matrix, columns) = sudokuToMatrix n initial

    let matrixChoices = matrix |> List.map (SudokuConverter.unMap n)

    printfn "rows: %i" (List.length matrix)

    let timey = System.Diagnostics.Stopwatch.StartNew ()

    let allSolutions = recursiveSolve matrix columns []

    timey.Stop ()

    for i in allSolutions |> solutionsToSudokus n initial do
        i |> printArray 0
        printfn ""

    printfn "\nsolutions: %i, time elapsed: %i" (allSolutions.Length) timey.ElapsedMilliseconds

    System.Console.ReadKey(true) |> ignore

 
let runWindow () =
    let window = new Window ()
    window.Show ()
    while window.Visible do
        window.Iterate ()
        window.Refresh ()
        System.Windows.Forms.Application.DoEvents ()

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let rules = { rotationsAllowed = true; reflectionsAllowed = true }

    //let dedend = {matrix = [[0;1;2;]]; matrixColumns = [0;1;2;3;]; children = []; deadEnd = true}
    //let posible = {matrix = [[0;1;2;]]; matrixColumns = [0;1;2;]; children = []; deadEnd = false}
    //let nodey = {matrix = [[3; 4; 5]; [2;3;4]; [0;1;2]]; matrixColumns = [0; 1; 2; 3; 4; 5;]; children = [dedend; dedend; posible]; deadEnd = false}

    //matrixSolveAndPrint argv rules
    //bruteForceAndAlsoPrintSolutionStuff argv rules
    recursiveMatrixSolveAndPrint argv rules
    //solveAll rules
    //runWindow ()
    //solveSudoku ()

    0 // return an integer exit code

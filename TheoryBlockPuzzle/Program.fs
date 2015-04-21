open Puzzle
open MatrixSolver
open RecursiveSolver
open MatrixPrinter
open BlockUtil
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

    printfn "\nisomorphic solutions: %i solutions: %i, time elapsed: %i" (allSolutions.Length) (solutions.Length) timey.ElapsedMilliseconds

    System.Console.ReadKey(true) |> ignore


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let rules = { rotationsAllowed = true; reflectionsAllowed = true }

    //let dedend = {matrix = [[0;1;2;]]; matrixColumns = [0;1;2;3;]; children = []; deadEnd = true}
    //let posible = {matrix = [[0;1;2;]]; matrixColumns = [0;1;2;]; children = []; deadEnd = false}
    //let nodey = {matrix = [[3; 4; 5]; [2;3;4]; [0;1;2]]; matrixColumns = [0; 1; 2; 3; 4; 5;]; children = [dedend; dedend; posible]; deadEnd = false}

    let evenBoard = array2D [| [|'1'; '2'|]; [|'3'; '4'|] |]

    let oddBoard = array2D [| [|'1'; '2'; '3'|]; [|'4'; '5'; '6'|]; [|'7'; '8'; '9'|] |]

    matrixSolveAndPrint argv rules
    //bruteForceAndAlsoPrintSolutionStuff argv rules
    //recursiveMatrixSolveAndPrint argv rules

    0 // return an integer exit code

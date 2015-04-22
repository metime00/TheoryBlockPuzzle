module MatrixPrinter
open MatrixSolver
open BlockUtil

let blockLocIsBlock rules blockLoc block =
    let baseBlock = (block |> List.map (fun (x, c) -> (x, 'c')))
    let blockConfigs =
        [
            if rules.rotationsAllowed && rules.reflectionsAllowed then //handles reflections and reflections of rotations
                let reflectedBlock = reflect Reflection.X baseBlock
                yield reflectedBlock
                yield rotate Rotation.Quarter reflectedBlock
                yield rotate Rotation.Half reflectedBlock
                yield rotate Rotation.ThreeQuarter reflectedBlock
            elif rules.reflectionsAllowed then //handles reflections
                yield reflect Reflection.X baseBlock
                yield reflect Reflection.Y baseBlock
            if rules.rotationsAllowed then //handles rotations
                yield rotate Rotation.Quarter baseBlock
                yield rotate Rotation.Half baseBlock
                yield rotate Rotation.ThreeQuarter baseBlock
            yield baseBlock
        ] |> List.map Set.ofList
    let blockLocBlock = blockLoc |> List.map (fun x -> ((x, 'c') : Tile)) |> zeroBlock |> Set.ofList
    List.contains blockLocBlock blockConfigs

/// given a target, block count, and solution, prints the solution's rows
let printSolution target blockCount (sol : int list list) =
    let partial = Array2D.create (Array2D.length1 target) (Array2D.length2 target) ' '
    for row in sol do //let be visible only the tiles that are filled in the partial solution
        for i in row do
            match unMap blockCount (Array2D.length1 partial) i with
            | Some((x, y)) ->
                partial.[x, y] <- target.[x, y]
            | None ->
                ()
    partial |> printArray 0

let toFileMatrixAsListOfLists (matrix : ChoiceMatrix) =
    let outFile = new System.IO.StreamWriter("problemset.txt")

    let rowString (row : int list) =
        let mutable out = "["
        for i = 0 to row.Length - 1 do
            if i < row.Length - 1 then
                out <- out + row.[i].ToString () + ", "
            else
                out <- out + row.[i].ToString ()
        out <- out + "]"
        out
    let mutable stringy = "["
    for i = 0 to matrix.Length - 1 do
        if i < matrix.Length - 1 then
            stringy <- stringy + rowString matrix.[i] + ", "
        else
            stringy <- stringy + rowString matrix.[i]
    stringy <- stringy + "]"

    outFile.Write(stringy)
    outFile.Close ()

/// counts the number of nodes
let rec countNodes tree =
    if tree.children = [] then 1
    else List.sumBy (fun x -> countNodes x) tree.children

/// returns a list of block locations, with order corresponding to given solutions and their corresponding blocks
let blockLoc target (blocks : Tile list list) rules (solution : int list list) =
    [
        for i in blocks do
            for row in solution do
                let blockLoc = row |> List.map (unMap blocks.Length (Array2D.length1 target)) |> List.filter Option.isSome |> List.map Option.get
                if blockLocIsBlock rules blockLoc i then
                    yield blockLoc
    ]

/// returns a list of char arrays given a list of block location solutions
let blockVis target (blockLoc : (int * int) list list) =
    let visArray = Array2D.create (Array2D.length1 target) (Array2D.length2 target) ' '
    for index = 0 to blockLoc.Length - 1 do
        let block = blockLoc.[index]
        for (i, j) in block do
            visArray.[i, j] <- char (index + 48)
    visArray

/// counts the number of solutions, including isomorphic ones
let rec countIsoSolutions tree =
    if tree.matrixColumns = [] then 1
    elif tree.children = [] then 0
    else List.sumBy (fun x -> countIsoSolutions x) tree.children

/// gets a list of solutions from a tree
let rec solutionsFromTree tree =
    if tree.matrixColumns = [] then [tree.partialSolution.Value]
    elif tree.children = [] then []
    else [ for i in tree.children do yield! solutionsFromTree i ]

/// returns a list of all solutions as a selection of rows, removing isomorphic ones
let matrixSolutionList rules target blocks solutions =
    
    let unfiltered = solutions |> List.map (blockLoc target blocks rules) |> List.map (blockVis target)
    /// a recursive function that takes the head, creates all rotations and reflections, filters out any of them from the list, then calls itself again until nothing happens. Then only non-isomorphic solutions remain
    let rec removeIsomorphs (solutionList : char[,] list) solutions =
        match solutionList with
        | curSol :: tail ->
            let isomorphs =
                [
                    let reflectedX = curSol |> arrayToBlock |> reflect Reflection.X |> blockToArray
                    let reflectedY = curSol |> arrayToBlock |> reflect Reflection.Y |> blockToArray
                    yield reflectedX |> arrayToBlock |> rotate Rotation.Quarter |> blockToArray
                    yield reflectedX |> arrayToBlock |> rotate Rotation.Half |> blockToArray
                    yield reflectedX |> arrayToBlock |> rotate Rotation.ThreeQuarter |> blockToArray
                    yield reflectedY |> arrayToBlock |> rotate Rotation.Quarter |> blockToArray
                    yield reflectedY |> arrayToBlock |> rotate Rotation.Half |> blockToArray
                    yield reflectedY |> arrayToBlock |> rotate Rotation.ThreeQuarter |> blockToArray
                    yield reflectedX
                    yield reflectedY
                    yield curSol |> arrayToBlock |> rotate Rotation.Half |> blockToArray
                    yield curSol |> arrayToBlock |> rotate Rotation.Quarter |> blockToArray
                    yield curSol |> arrayToBlock |> rotate Rotation.ThreeQuarter |> blockToArray
                ]
            let newSolutions = tail |> List.filter (fun x -> not (List.contains x isomorphs))
            removeIsomorphs newSolutions (curSol :: solutions)
        | [] ->
            solutions
    removeIsomorphs unfiltered []
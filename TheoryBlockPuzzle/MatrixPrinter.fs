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

/// given a target
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

/// counts the number of solutions
let rec countMatrixSolutions tree =
    if tree.matrixColumns = [] then 1
    elif tree.children = [] then 0
    else List.sumBy (fun x -> countMatrixSolutions x) tree.children

/// returns a list of all solutions as a selection of rows
let rec matrixSolutionList tree =
    if tree.matrixColumns = [] then [tree.partialSolution.Value]
    elif tree.children = [] then []
    else [ for i in tree.children do yield! matrixSolutionList i ]

/// returns a list of block locations, with order corresponding to given solutions and their corresponding blocks
let blockLoc target (blocks : Tile list list) rules (solutions : int list list list) =
    [
        for sol in solutions do
            yield
                [
                    for i in blocks do
                        for row in sol do
                            let blockLoc = row |> List.map (unMap blocks.Length (Array2D.length1 target)) |> List.filter Option.isSome |> List.map Option.get
                            if blockLocIsBlock rules blockLoc i then
                                yield blockLoc
                ]
    ]

/// returns a list of char arrays given a list of block location solutions
let blockVis target (blockLoc : (int * int) list list) =
    let visArray = Array2D.create (Array2D.length1 target) (Array2D.length2 target) ' '
    for index = 0 to blockLoc.Length - 1 do
        let block = blockLoc.[index]
        for (i, j) in block do
            visArray.[i, j] <- char (index + 48)
    visArray
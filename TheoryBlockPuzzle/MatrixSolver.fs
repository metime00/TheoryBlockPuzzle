module MatrixSolver
open BlockUtil

type coord = (int * int)

type colLoc = int

/// a list of row or column name by rows or columns that it has a 1 in
type ChoiceMatrix = (int * int list) list

module List =
    /// Tests if an element is contained in a list
    let contains element list =
        List.exists (fun i -> i = element) list
    /// Tests if any elements are shared by two lists
    let intersects list1 list2 =
        List.exists (fun x  -> contains x list2) list1

/// matrix columns are the names of columns that exist in the current node. For example, there could be three columns in the submatrix, but the names of those columns could be 1, 22, and 24. They still have to be ordered
type Node = { matrixRows : ChoiceMatrix; matrixColumns : ChoiceMatrix; children : Node list; partialSolution : int list list option } //partialSolution is a list of rows chosen to be in the partial solution

/// a mapping of x y coordinates to a natural number based on an offset (blockCount) and the size of the board
let inline mapping blockCount (width : int) ((x : int), (y : int)) =
    blockCount + y * width + x

/// reverses the mapping, returning x, y coordinate that a mapping corresponds to
let inline unMap (blockCount : int) (width : int) (mapped : int) =
    if mapped < blockCount then
        None
    else
        let offset = mapped - blockCount
        let x = offset % width
        let y = offset / width
        Some((x, y))

/// Given a block and a target, gives a list of all the board configurations that can be made by placing the block that won't automatically be wrong, configured as a list of x y coordinates for each placement
let placeBlocks (block : Tile list) target =
    [
        for i = 0 to Array2D.length1 target - 1 do
            for j = 0 to Array2D.length2 target - 1 do
                if List.forall (fun ((x, y), c) -> i + x < Array2D.length1 target && j + y < Array2D.length2 target && target.[i + x, j + y] = c) block then
                    let row = 
                        [
                            for ((x, y), _) in block do
                                yield (i + x, j + y)
                        ]
                    yield row
    ]

/// Creates a choice/constraint matrix (in the form of a list) from a puzzle, reducing the number of choices by if the characters in the possible choices match the target
let createMatrix (target, (blocks : Tile list list)) rules =
    let rows =
        [
            for i = 0 to blocks.Length - 1 do
                // all the valid placements based on if reflections and rotations are allowed, as a list of placement coordinates
                let validPlacements = 
                    [
                        if rules.rotationsAllowed && rules.reflectionsAllowed then //handles reflections and reflections of rotations
                            let reflectedBlock = reflect Reflection.X blocks.[i]
                            yield! placeBlocks (reflectedBlock) target
                            yield! placeBlocks (rotate Rotation.Quarter reflectedBlock) target
                            yield! placeBlocks (rotate Rotation.Half reflectedBlock) target
                            yield! placeBlocks (rotate Rotation.ThreeQuarter reflectedBlock) target
                        elif rules.reflectionsAllowed then //handles reflections
                            yield! placeBlocks (reflect Reflection.X blocks.[i]) target
                            yield! placeBlocks (reflect Reflection.Y blocks.[i]) target
                        if rules.rotationsAllowed then //handles rotations
                            yield! placeBlocks (rotate Rotation.Quarter blocks.[i]) target
                            yield! placeBlocks (rotate Rotation.Half blocks.[i]) target
                            yield! placeBlocks (rotate Rotation.ThreeQuarter blocks.[i]) target
                        yield! placeBlocks blocks.[i] target
                    ] |> List.map Set.ofList |> Set.ofList |> Set.toList |> List.map Set.toList
                //take the coordinates, and turn them into the column locations in the matrix based on the mapping function of block count, board size, and coordinates of the filled tiles
                for row in validPlacements do
                    yield i :: List.map (mapping blocks.Length (Array2D.length1 target)) row // the list of all the (x, y) coordinates satisfied by the specific block rotation, reflection and placement choice
        ]
    let notBlank colMap = //tests if a column maps to an x y coordinate of the target board that doesn't need to be filled with anything
        match unMap blocks.Length (Array2D.length1 target) colMap with
        | Some((x, y)) ->
            target.[x, y] <> ' '
        | None ->
            true
    let columnNames = [0 .. List.maxBy (fun row -> List.max row) rows |> List.max] |> List.filter (notBlank)
    let rowNames = [0 .. rows.Length - 1]
    let columns = columnNames |> List.map (fun col -> rowNames |> List.filter (fun rowName -> List.contains col rows.[rowName]))
    let (rowMatrix : ChoiceMatrix) = List.map2 (fun name row -> (name, row)) rowNames rows
    let (colMatrix : ChoiceMatrix) = List.map2 (fun name col -> (name, col)) columnNames columns
    (rowMatrix, colMatrix)

/// Performs one iteration of algorithm x, returning the tree of partial and full solutions, and the current partial solution, so that it can be drawn
let iterateX tree nextEdges =
    ///Returns the next node to perform an iteration of algorithm x on, and the list of edge nodes
    let rec filterNode nodeList =
        match nodeList with
        | node :: tail ->
            if Option.isNone node.partialSolution then filterNode tail
            elif node.matrixColumns = [] then filterNode tail
            else
                match node.children with
                | [] -> (Some(node), tail)
                | children -> filterNode tail
        | [] -> (None, [])

    let (node, edges) = filterNode nextEdges
    match node with
    | Some(curNode) ->
        /// builds a tree to be returned with one iteration of algorithm x performed on it
        let rec buildTree oldTree node =
            let nextSolIndex = node.partialSolution.Value.Length - oldTree.partialSolution.Value.Length - 1
            if nextSolIndex <> -1 then
                let nextSol = node.partialSolution.Value.[nextSolIndex]
                let (changedNode, newEdges) = 
                    let nodeToChange = oldTree.children |> List.pick (fun x -> match x.partialSolution with Some(head :: _) when head = nextSol -> Some(x) | _ -> None)
                    buildTree nodeToChange node
                ({matrixRows = oldTree.matrixRows; matrixColumns = oldTree.matrixColumns; children = oldTree.children |> List.map (fun x -> match x.partialSolution with Some(head :: _) when head = nextSol -> changedNode | _ -> x); partialSolution = oldTree.partialSolution }, newEdges)
            else //perform one iteration of algorithm x
                let minColumnOnes = //the smallest number of ones in any column
                    node.matrixColumns |> List.map (fun (_, column) -> column.Length) |> List.min
                if minColumnOnes = 0 then //if there are any columns with no 1s, then this partial solution won't work
                    ({matrixRows = node.matrixRows; matrixColumns = node.matrixColumns; children = []; partialSolution = None}, edges)
                else
                    let columnName = node.matrixColumns |> List.pick (fun (name, rows) -> if rows.Length = minColumnOnes then Some(name) else None) //the name of the column that is the first column with the minimum number of 1s
                    let newChildren = //select rows to branch nondeterministically
                        [
                            for (rowName, row) in List.filter (fun (_, row) -> List.contains columnName row) node.matrixRows do
                                let newMatrixColumns = node.matrixColumns |> List.filter (fun (_, column) -> not (List.contains rowName column)) //remove columns that the row has a 1 in
                                let newMatrixRows = node.matrixRows |> List.filter (fun (_, x) -> not (List.intersects x row)) //remove rows that have 1s in columns that the chosen row has 1s in
                                yield {matrixRows = newMatrixRows; matrixColumns = newMatrixColumns; children = []; partialSolution = Some(row :: Option.get node.partialSolution)} //yield new branches
                        ]
                    ({matrixRows = node.matrixRows; matrixColumns = node.matrixColumns; children = newChildren; partialSolution = node.partialSolution}, newChildren @ edges)
        let (newTree, newEdges) = buildTree tree curNode
        (newTree, curNode.partialSolution, newEdges)
    | None ->
        (tree, None, [])

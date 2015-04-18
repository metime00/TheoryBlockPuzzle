module Puzzle
open BlockUtil

/// a record of a board's state and the blocks left to place
type PuzzleState = { board : char[,]; blocks : Tile list list }
/// a record of the puzzle's state, and the nodes that derive directly from that state.
type PuzzleNode = { puzzle : PuzzleState; children : PuzzleNode list }

/// recursively print the boards of every node and its children 
let rec printPuzzleNode level node =
    printArray level node.puzzle.board
    printfn ""
    List.iter (printPuzzleNode (level + 1)) node.children

let rec countSolutions target node =
    if node.puzzle.board = target then 1
    else List.sum [for i in node.children do yield countSolutions target i]

/// Given a block and a board, gives a list of all the board configurations that can be made by placing the block
let placeBlocks (block : Tile list) board =
    [
        for i = 0 to Array2D.length1 board - 1 do
            for j = 0 to Array2D.length2 board - 1 do
                if List.forall (fun ((x, y), _) -> i + x < Array2D.length1 board && j + y < Array2D.length2 board && board.[i + x, j + y] = ' ') block then
                    let newBoard = Array2D.copy board
                    List.iter (fun ((x, y), c) -> newBoard.[i + x, j + y] <- c) block
                    yield newBoard
    ]

/// tells whether or not a board and a target have any mismatched, non blank tiles, true says to stop short, false means it's still possible
let shortStop (target : char[,]) (board : char[,]) =
    let mutable output = false
    for i = 0 to Array2D.length1 target - 1 do
            for j = 0 to Array2D.length2 target - 1 do
                if target.[i, j] <> board.[i, j] && board.[i, j] <> ' ' then output <- true
    output
/// Given a target board, current board configuration, and remaining blocks, tells whether a board is impossible to reach the target state with, true means possible, false means impossible
let boardFilter target (blocks : Tile list list) board =
    if board = target then true
    elif blocks = [] then false
    elif placeBlocks blocks.[blocks.Length - 1] board = [] then false
    elif shortStop target board then false
    else true

/// returns the 
let bruteSolve target (blocks : Tile list list) rules =
    let emptyBoard = Array2D.create (Array2D.length1 target) (Array2D.length2 target) ' '
    let initialState = { board = emptyBoard; blocks = blocks }

    let rec evaluate (graph : PuzzleState) =
        match graph.blocks with
        | head :: tail ->
            let nextStates =
                let nextBoards = 
                    [
                        if rules.rotationsAllowed then
                            yield! placeBlocks (rotate Rotation.Quarter graph.blocks.Head) graph.board
                            yield! placeBlocks (rotate Rotation.Half graph.blocks.Head) graph.board
                            yield! placeBlocks (rotate Rotation.ThreeQuarter graph.blocks.Head) graph.board
                        if rules.reflectionsAllowed then
                            yield! placeBlocks (reflect Reflection.X graph.blocks.Head) graph.board
                            yield! placeBlocks (reflect Reflection.Y graph.blocks.Head) graph.board
                        yield! placeBlocks graph.blocks.Head graph.board

                    ]
                nextBoards
                |> List.filter (boardFilter target graph.blocks.Tail)
                |> List.map (fun x -> { board = x; blocks = graph.blocks.Tail })

            { puzzle = graph; children = [ for i in nextStates do yield evaluate i ] }
        | [] -> { puzzle = graph; children = [] }
    let filledGraph = evaluate initialState 
    filledGraph
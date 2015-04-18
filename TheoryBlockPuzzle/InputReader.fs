module InputReader

open BlockUtil

open System
open System.IO
open System.Collections.Generic

/// returns a list of valid adjacent x y coordinates given an x and y bound
let adjacent sizeX sizeY (x, y) =
    [
        for (i, j) in [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1); (x, y)] do
            if i >= 0 && i < sizeX && j >= 0 && j < sizeY then yield (i, j)
    ]

let fileToArray (filename : string) =
    let lines = [
        use sr = new StreamReader (Environment.CurrentDirectory + "\\" + filename)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    ]
    let width = List.maxBy (fun x -> String.length x) lines |> String.length
    let height = List.length lines
    let output = Array2D.create width height ' '
    for j = 0 to lines.Length - 1 do
        for i = 0 to String.length lines.[j] - 1 do
            output.[i, j] <- lines.[j].[i]
    output

/// Takes an array read in from an input file and returns the target board and a list of all the blocks ordered by size
let arrayToPuzzle input =
    let blocks = new List<Tile Set> ()
    ///returns a tuple of the next block
    let rec floodFill board (tiles : (int * int) Set) =
        let nextTiles =
            [for i in tiles do yield! adjacent (Array2D.length1 board) (Array2D.length2 board) i]
            |> Set.ofList
            |> Set.filter (fun (x, y) -> board.[x, y] <> ' ')
        if nextTiles.Count > tiles.Count then
            floodFill board nextTiles
        else
            tiles |> Set.map (fun (x, y) -> (((x, y), board.[x, y]) : Tile))
    //iterates over the board, modifying it and returning a list of all blocks
    for i = 0 to Array2D.length1 input - 1 do
        for j = 0 to Array2D.length2 input - 1 do
            if input.[i, j] <> ' ' then
                let nextBlock = floodFill input (set [(i, j)])
                for ((x, y), _) in nextBlock do
                    input.[x, y] <- ' '
                blocks.Add nextBlock
    //blocks sorted by descending order of size
    let sortedBlocks =
        blocks.ToArray ()
        |> List.ofArray
        |> List.sortBy (fun x -> x.Count)
        |> List.rev
        |> List.map (fun x -> Set.toList x)

    //this is the part where the blocks are all centered with their upper left corner as the origin
    let puzzle = sortedBlocks |> List.tail |> List.map (fun x -> zeroBlock x)
    //this is the part where the target board block is changed to be a 2d array
    let target = sortedBlocks |> List.head |> blockToArray
    (target, puzzle)
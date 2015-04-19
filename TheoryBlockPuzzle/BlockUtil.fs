module BlockUtil

type GameRules = { rotationsAllowed : bool; reflectionsAllowed : bool }

type Tile = (int * int) * char

type Rotation =
    | Quarter
    | Half
    | ThreeQuarter

type Reflection =
    | X
    | Y

/// makes all tiles in a block relativized with the upper left being the origin
let zeroBlock (block : Tile list) =
    let xMin = List.minBy (fun ((x, y), _) -> x) block |> fst |> fst
    let yMin = List.minBy (fun ((x, y), _) -> y) block |> fst |> snd
    [
        for ((x, y), c) in block do yield (((x - xMin, y - yMin), c) : Tile)
    ]

/// takes an array and transforms it into a block of all the non blank characters
let arrayToBlock board =
    [
        for i = 0 to Array2D.length1 board - 1 do
            for j = 0 to Array2D.length2 board - 1 do
                if board.[i, j] <> ' ' then yield (((i, j), board.[i, j]) : Tile)
    ]

/// takes a block, zeros it, then converts it into an array where unfilled tiles are the space character
let blockToArray (block : Tile list) =
    let filledTiles = block |> zeroBlock
    let xMax = List.maxBy (fun ((x, y), _) -> x) filledTiles |> fst |> fst
    let yMax = List.maxBy (fun ((x, y), _) -> y) filledTiles |> fst |> snd
    let out = Array2D.create (xMax + 1) (yMax + 1) ' '
    List.iter (fun ((x, y), c) -> out.[x, y] <- c) filledTiles
    out

/// rotate clockwise by the amount given by the degree, and zero it
let rotate degree (block : Tile list) =
    let rotatedBlock =
        match degree with
        | Rotation.Quarter ->
            List.map (fun ((x, y), c) -> (((y, -x), c) : Tile)) block
        | Rotation.Half ->
            List.map (fun ((x, y), c) -> (((-x, -y), c) : Tile)) block
        | Rotation.ThreeQuarter ->
            List.map (fun ((x, y), c) -> (((-y, x), c) : Tile)) block
    zeroBlock rotatedBlock

/// reflect the block about the given axis, and zero it
let reflect axis (block : Tile list) = 
    let reflectedBlock =
        match axis with
        | X ->
            List.map (fun ((x, y), c) -> (((x, -y), c) : Tile)) block
        | Y ->
            List.map (fun ((x, y), c) -> (((-x, y), c) : Tile)) block
    zeroBlock reflectedBlock

let printArray tabs input =
    for j = 0 to Array2D.length2 input - 1 do
        for i = 0 to tabs - 1 do printf "   "
        for i = 0 to Array2D.length1 input - 1 do
            printf "%c" input.[i, j]
        printf "\n"
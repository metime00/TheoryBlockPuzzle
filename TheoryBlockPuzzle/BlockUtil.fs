module BlockUtil

type GameRules = { rotationsAllowed : bool; reflectionsAllowed : bool }

type Tile = (int * int) * char

module List =
    /// Tests if an element is contained in a list
    let contains element list =
        List.exists (fun i -> i = element) list
    /// Tests if any elements are shared by two lists
    let intersects list1 list2 =
        List.exists (fun x  -> contains x list2) list1

type Rotation =
    | Quarter
    | Half
    | ThreeQuarter

type Reflection =
    | X
    | Y

let rec factorial x =
    match x with
    | 1 -> 1
    | _ -> x * factorial (x - 1)

/// makes all tiles in a block relativized with the upper left being the origin
let zeroBlock (block : Tile list) =
    let xMin = List.minBy (fun ((x, y), _) -> x) block |> fst |> fst
    let yMin = List.minBy (fun ((x, y), _) -> y) block |> fst |> snd
    [
        for ((x, y), c) in block do yield (((x - xMin, y - yMin), c) : Tile)
    ]

/// gets a row from a 2d array
let Array2DRow row input =
    [
        for i = 0 to Array2D.length1 input - 1 do
            yield input.[i,row]
    ]

/// gets a column from a 2d array
let Array2DColumn col input =
    [
        for i = 0 to Array2D.length2 input - 1 do
            yield input.[col,i]
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

/// tests if two blocks are the same, given allowed rotations or reflections
let equal (block1 : Tile list) (block2 : Tile list) rules =
    let block1Permutations =
        [
            if rules.rotationsAllowed && rules.reflectionsAllowed then //handles reflections and reflections of rotations
                let reflectedBlock = reflect Reflection.X block1
                yield reflectedBlock
                yield rotate Rotation.Quarter reflectedBlock
                yield rotate Rotation.Half reflectedBlock
                yield rotate Rotation.ThreeQuarter reflectedBlock
            elif rules.reflectionsAllowed then //handles reflections
                yield reflect Reflection.X block1
                yield reflect Reflection.Y block1
            if rules.rotationsAllowed then //handles rotations
                yield rotate Rotation.Quarter block1
                yield rotate Rotation.Half block1
                yield rotate Rotation.ThreeQuarter block1
            yield block1
        ]
    List.contains block2 block1Permutations

///given a list of blocks, returns the ones that have duplicates, and how many duplicates there are
let identicalBlocks (blocks : Tile list list) rules =
    let rec pickBlocks (recBlocks : Tile list list) identicalBlocks =
        match recBlocks with
        | head :: tail ->
            if tail |> List.exists (fun x -> equal head x rules) then
                pickBlocks tail (Set.add head identicalBlocks)
            else
                pickBlocks tail identicalBlocks
        | [] ->
            identicalBlocks
    let duplicatedBlocks = (pickBlocks blocks Set.empty) |> Set.toList
    duplicatedBlocks |> List.map (fun x -> (x, blocks |> List.filter (fun y -> equal x y rules) |> List.length))
        

let printArray tabs input =
    for j = 0 to Array2D.length2 input - 1 do
        for i = 0 to tabs - 1 do printf "   "
        for i = 0 to Array2D.length1 input - 1 do
            printf "%c" input.[i, j]
        printf "\n"
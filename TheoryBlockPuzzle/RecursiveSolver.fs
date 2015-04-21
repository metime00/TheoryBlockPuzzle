module RecursiveSolver
open MatrixSolver

let rec recursiveSolve (input : ChoiceMatrix) (columnNames) (partialSolution : int list list) =
    if input = [] && columnNames = [] then
        [partialSolution]
    elif input = [] && columnNames <> [] then
        []
    else
        let columnOnes = columnNames |> List.map (fun col -> input |> List.sumBy (fun row -> if List.contains col row then 1 else 0))
        let leastOnes = columnOnes |> List.min
        let column = columnNames.[List.findIndex (fun x -> x = leastOnes) columnOnes]
        if leastOnes = 0 then
            []
        else
            [
                for row in List.filter (List.contains column) input do
                    let newMatrix = input |> List.filter (fun x -> not (List.intersects x row)) //remove rows that have 1s in columns that the chosen row has 1s in
                    let newColumns = columnNames |> List.filter (fun x -> not (List.contains x row)) //remove columns that the row has a 1 in
                    match recursiveSolve newMatrix newColumns (row :: partialSolution) with
                    | [] -> ()
                    | solution -> yield! solution
            ]
module Window

open MatrixSolver
open BlockUtil
open InputReader
open Puzzle
open RecursiveSolver
open SudokuConverter
open MatrixPrinter

open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms
open System.Windows

type WindowState =
    | Waiting //this is where the user tells the file to read and the rules to have and the algorithm to use
    | GraphicalSolver // solves a block puzzle, and has two flags, whether or not to step by step solve, waiting for user input, or to graphically display solution

type Window () as this =
    inherit Form ()

    let mutable state = WindowState.Waiting

    let size = new Size(640, 480)

    let display = new TextBox ()
    let console = new TextBox ()

    let timey = new System.Diagnostics.Stopwatch ()

    let rotations = new CheckBox ()
    let reflections = new CheckBox ()
    let singleStep = new CheckBox ()

    let mutable tree = {matrix = []; matrixColumns = []; children = []; partialSolution = Some([])}
    let mutable nextEdges = []
    let mutable canStep = false
    let mutable target = None
    let mutable blocks = None
    let mutable rules = None

    let changeToGraphicalSolver args =
        state <- WindowState.GraphicalSolver

        let filename = console.Text
        rules <- Some({ rotationsAllowed = rotations.Checked; reflectionsAllowed = reflections.Checked })

        let writeums = fileToArray filename
        let (tempTar, tempBlo) = arrayToPuzzle writeums
        target <- Some(tempTar)
        blocks <- Some(tempBlo)

        let (matrix, columns) = createMatrix (target.Value, blocks.Value) rules.Value
        tree <- {matrix = matrix; matrixColumns = columns; children = []; partialSolution = Some([])}
        nextEdges <- [tree]

        timey.Start ()

        this.Controls.Clear ()

        display.AutoSize <- false
        display.Size <- new Size(480, 200)
        display.Location <- new Point(0, 0)
        display.ReadOnly <- true
        display.Multiline <- true

        console.Clear ()
        console.AutoSize <- false
        console.Size <- new Size(480, 50)
        console.Location <- new Point(0, 250)
        console.Multiline <- true

        singleStep.Location <- new Point(0,300)
        singleStep.Text <- "single step through the solver"

        let singleStepButton = new Button ()
        singleStepButton.Location <- new Point(100, 300)
        singleStepButton.Text <- "step"
        singleStepButton.Click.Add (fun args -> canStep <- true)

        this.Controls.AddRange [| singleStep; display; console; singleStepButton |]

    let changeToWaiting () =
        this.Controls.Clear ()
        display.AutoSize <- false
        display.Size <- new Size(480, 200)
        display.Location <- new Point(0, 0)
        display.ReadOnly <- true
        display.Font <- new Font("Consolas", 8.0f)

        console.AutoSize <- false
        console.Size <- new Size(480, 50)
        console.Location <- new Point(0, 250)
        console.Font <- new Font("Consolas", 8.0f)

        let graphicSolveButton = new Button ()
        graphicSolveButton.AutoSize <- true
        graphicSolveButton.Location <- new Point(0, 300)
        graphicSolveButton.Text <- "solve graphically"
        graphicSolveButton.Click.Add changeToGraphicalSolver

        let recursiveSolveButton = new Button ()
        recursiveSolveButton.AutoSize <- true
        recursiveSolveButton.Location <- new Point(150, 300)
        recursiveSolveButton.Text <- "solve recursively"
    
        let sudokuSolveButton = new Button ()
        sudokuSolveButton.AutoSize <- true
        sudokuSolveButton.Location <- new Point(300, 300)
        sudokuSolveButton.Text <- "solve a sudoku"

        reflections.Text <- "allow reflections"
        reflections.Location <- new Point(0, 350)
        reflections.Checked <- true
        rotations.Text <- "allow rotations"
        rotations.Location <- new Point(150, 350)
        rotations.Checked <- true

        this.Controls.AddRange [| display; console; graphicSolveButton; recursiveSolveButton; sudokuSolveButton; reflections; rotations |]

    let finish (solutions : char[,] list) consoleText =
        state <- WindowState.Waiting
        display.Clear ()
        for k in 0 .. 10 .. solutions.Length - 1 do 
            for i = 0 to Array2D.length1 solutions.[k] - 1 do
                for j = 0 to Array2D.length2 solutions.[k] - 1 do
                    display.AppendText (string (solutions.[k].[i, j]))
                display.AppendText(System.Environment.NewLine)
            display.AppendText(System.Environment.NewLine + System.Environment.NewLine)

        console.Text <- consoleText

    do
        this.Text <- "John Block Puzzle"
        this.ClientSize <- size
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.Tan
        changeToWaiting ()

    /// do a task based on what state the window is currently in
    member this.Iterate () =
        match state with
        | WindowState.GraphicalSolver ->
            let doStuff () =
                let (nextTree, partialSolution, newEdges) = iterateX tree nextEdges
                match partialSolution with
                | Some(sol) ->
                    let partial = solutionPuzzle target.Value blocks.Value.Length sol
                    display.Clear ()
                    for i = 0 to Array2D.length1 partial - 1 do
                        for j = 0 to Array2D.length2 partial - 1 do
                            display.AppendText (string (partial.[i, j]))
                        display.AppendText(System.Environment.NewLine)
                    tree <- nextTree
                    nextEdges <- newEdges

                    System.Threading.Thread.Sleep 10
                | None ->
                    let solutions = matrixSolutionList rules.Value target.Value blocks.Value (solutionsFromTree tree)

                    let numSolutions =
                        let divisor = 
                            match identicalBlocks blocks.Value rules.Value with
                            | [] -> 1
                            | x -> x |> List.map (fun (_, x) -> factorial x) |> List.reduce (fun x y -> x * y)
                        solutions.Length / divisor
                    timey.Stop ()
                    let consoleText = String.Format ("\nisomorphic solutions: {0} solutions: {1}, time elapsed: {2}", (solutions.Length), (numSolutions), timey.ElapsedMilliseconds)
                    timey.Reset ()
                    finish solutions consoleText
            if singleStep.Checked && canStep then
                doStuff ()
                canStep <- false
            elif not singleStep.Checked then
                doStuff ()
        | _ -> ()
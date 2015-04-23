module Window

open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

type WindowState =
    | Waiting //this is where the user tells the file to read and the rules to have and the algorithm to use
    | GraphicalSolver // solves a block puzzle, and has two flags, whether or not to step by step solve, waiting for user input, or to graphically display solution
    | RecursiveSolver // solves a block puzzle, but provides no user feedback
    | Finished //displays the solution(s) found, telling the number of solutions, isomorphic solutions, and how long it took

type Window () as this =
    inherit Form ()

    let size = new Size(640, 480)

    let endGame args =
        
        ()

    do
        this.Text <- "John Block Puzzle"
        this.ClientSize <- size
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.Tan
        

    override this.OnMouseMove args =
        ()
    
    override this.OnMouseDown args =
        ()      

    override this.OnPaint args =
        ()

    /// do a task based on what state the window is currently in
    member this.Iterate () =
        ()
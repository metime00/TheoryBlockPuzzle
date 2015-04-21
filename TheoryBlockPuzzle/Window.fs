module Window

open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

type Window () as this =
    inherit Form ()

    let endGame args =
        
        ()

    do
        this.Text <- "John Block Puzzle"
        this.ClientSize <- new Size(640, 480)
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
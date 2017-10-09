// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Windows.Forms

open Chess

[<EntryPoint>]
let main args = 
    
    let fenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" // w KQkq - 0 1"
    let fenEmpty = "8/8/8/8/8/8/8/8"
    
    let b = Chess.makeBoard fenStart
    Chess.printBoard b

    //let b' = Chess.move "e4" b

    0

    // while (1)
    // display board
    // read move
    // if "quit", quit app
    // else
    //     if legal make it and advance board state, display board
    //     if illegal print error and 

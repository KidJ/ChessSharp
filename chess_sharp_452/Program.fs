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

    Chess.tryMove "e2" "e4" b
    Chess.printBoard b

    Chess.tryMove "e7" "e5" b
    Chess.printBoard b

    Chess.tryMove "g1" "f3" b
    Chess.printBoard b

    Chess.tryMove "b8" "c6" b
    Chess.printBoard b

    Chess.tryMove "f1" "c4" b
    Chess.printBoard b

    Chess.tryMove "g8" "f6" b
    Chess.printBoard b

    // fried liver!
    Chess.tryMove "f3" "g5" b
    Chess.printBoard b

    0
    

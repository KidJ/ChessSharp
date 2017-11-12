// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Windows.Forms

open Chess

let playMoves (board : Board) (moves : Move list) =
    for move in moves do
        Chess.tryMove board (fst move) (snd move)
        Chess.printBoard board

let friedLiverMoves = 
    [
        ("e2", "e4");
        ("e7", "e5");
        ("g1", "f3");
        ("b8", "c6");
        ("f1", "c4");
        ("g8", "f6");
        ("f3", "g5")
    ]

let scandinavianMoves =
    [
        ("e2", "e4");
        ("d7", "d5");
        ("e4", "d5");
        ("d8", "d5");
        ("b1", "c3")
    ]

[<EntryPoint>]
let main args = 
    
    let fenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" // w KQkq - 0 1"
    let fenEmpty = "8/8/8/8/8/8/8/8"
    
    let board = Chess.makeBoard fenStart
    playMoves board scandinavianMoves

    0
    

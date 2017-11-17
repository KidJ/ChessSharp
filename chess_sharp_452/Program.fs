// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Chess

let playMoves (board : Board) (moves : Move list) =
    for move in moves do
        Chess.tryMove board move
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

let playConsole board =
    Chess.printBoard board

    let mutable line = ""
    
    while line <> "q" || line <> "quit" do
        line <- System.Console.ReadLine()
        printfn "Move \"%s\"." line
        let move = Move.tryParse line
        match move with
        | Some m -> 
            Chess.tryMove board m
            Chess.printBoard board
        | None -> printfn "Move \"%s\" not legal, please try again." line


//let line = System.Console.Readline()let playStdin board = 
    //let input = [0..N-1] |> Seq.map (fun _ -> Console.ReadLine())
    //let line = System.Console.Readline()
    
    //if Move.tryPase

[<EntryPoint>]
let main args = 

    let fenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" // w KQkq - 0 1"
    let fenEmpty = "8/8/8/8/8/8/8/8"
    
    let board = Chess.makeBoard fenStart

    playConsole board
    //playMoves board scandinavianMoves

    0
    

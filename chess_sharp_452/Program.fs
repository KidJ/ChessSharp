// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Chess

let playMoves (board : Board) (moves : Move list) =
    let mutable b = board
    for move in moves do
        match Chess.tryApplyMove board move with
        | Some nextBoard ->
            Chess.printBoard board
            b <- nextBoard
        | None -> 
            printfn "Illegal move %A" move

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

let fetchLegalMoveFromConsole () : Move option =
    let mutable input = ""
    let mutable move : Chess.Move option = None
    while move.IsNone && input <> "quit" do
        input <- System.Console.ReadLine()
        if input <> "quit" then
            move <- Move.tryParse input
            if move.IsNone then
                printfn "Input \"%s\" not a valid move." input
    move

let consoleMoveSource (board : Board) : Move option =
    fetchLegalMoveFromConsole()

let randomValidMoveSource (board : Board) : Move option =
    let moves = generateValidMoves board
    if moves.Length = 0 then
        None
    else 
        let random = new System.Random(System.DateTime.Now.Millisecond)
        Some moves.[ random.Next(0, moves.Length) ]

let playGame (startBoard : Board) whiteMoveSource blackMoveSource = 
    Chess.printBoard startBoard

    printfn "GAME ON"
    let mutable boards = [ startBoard ]
    let mutable quit = false

    while not ((List.head boards).IsGameOver()) && not quit do
        let board = List.head boards

        // whose move is it?
        let moveColour = turnColour board
        let moveSource = if moveColour = PieceColour.White then whiteMoveSource else blackMoveSource
        printfn "%A to move" moveColour

        // get move from move source
        match moveSource board with
        | Some move ->
            match Chess.tryApplyMove board move with
            | Some nextBoard ->
                printfn "Move %A." move
                Chess.printBoard nextBoard
                boards <- nextBoard :: boards
            | None -> 
                printfn "Invalid move %A, please try another." move
        | None ->
            quit <- true
    
    printfn "GAME OVER"
    
let playConsole2Player board =
    playGame board consoleMoveSource consoleMoveSource

let playConsolePlayerVersusRandomAI board =
    playGame board consoleMoveSource randomValidMoveSource
    
[<EntryPoint>]
let main args = 

    let fenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" // w KQkq - 0 1"
    let fenEmpty = "8/8/8/8/8/8/8/8"
    
    let board = Chess.makeBoard fenStart

    playConsolePlayerVersusRandomAI board
    //playConsole2Player board
    //playConsole board
    //playMoves board scandinavianMoves

    0
    

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

let fetchLegalMoveFromConsole () : Move =
    let mutable input = ""
    let mutable validMove = false
    let mutable move : Chess.Move option = None
    while move.IsNone do
        input <- System.Console.ReadLine()
        move <- Move.tryParse input
        if move.IsNone then
            printfn "Input \"%s\" not a valid move, please try again." input
    
    move.Value

let consoleMoveSource (board : Board) : Move =
    fetchLegalMoveFromConsole()

let randomValidMoveSource (board : Board) : Move =
    let moves = generateValidMoves board
    if moves.Length = 0 then failwith "Unable to find valid move!"
    let random = new System.Random(System.DateTime.Now.Millisecond)
    moves.[ random.Next(0, moves.Length) ]

let playGame (board : Board) whiteMoveSource blackMoveSource =
    
    printfn "GAME ON"
    Chess.printBoard board

    while not (board.IsGameOver ()) do
        // whose move is it?
        let moveColour = turnColour board
        let moveSource = if moveColour = PieceColour.White then whiteMoveSource else blackMoveSource
        printfn "%A to move" moveColour

        // get move from move source
        let move = moveSource board
        let validMove = Chess.tryMove board move
        if validMove then
            printfn "Move %s." (string move)
            Chess.printBoard board
        else
            printfn "Invalid move %A, please try another." move

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
    

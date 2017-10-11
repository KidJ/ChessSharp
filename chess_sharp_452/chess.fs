module Chess

    type PieceType =
        | Pawn
        | Knight
        | Bishop
        | Rook
        | Queen
        | King
    
    type PieceColour = 
        | White
        | Black

    type Piece =
        {
            pieceType : PieceType
            colour : PieceColour
        }
    
    type Square =
        {
            file: int
            rank : int
        }
    
    let squareNames = 
        [|
            "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8";
            "b1"; "b2"; "b3"; "b4"; "b5"; "b6"; "b7"; "b8";
            "c1"; "c2"; "c3"; "c4"; "c5"; "c6"; "c7"; "c8";
            "d1"; "d2"; "d3"; "d4"; "d5"; "d6"; "d7"; "d8";
            "e1"; "e2"; "e3"; "e4"; "e5"; "e6"; "e7"; "e8";
            "f1"; "f2"; "f3"; "f4"; "f5"; "f6"; "f7"; "f8";
            "g1"; "g2"; "g3"; "g4"; "g5"; "g6"; "g7"; "g8";
            "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "h7"; "h8";
        |]

    // flatten Square to array index
    //let flatten square : int = 
    //    square.rank * 8 + square.file

    //// unflatten index to Square
    //let unflatten index : Square =
    //    assert ((index > -1) && (index < 64))
    //    { rank = index / 8; file = index % 8 }

    //let toString bsq = squareNames.[flatten bsq]

    let toIndex (squareName : string) =
        Array.tryFindIndex (fun s -> s = squareName) squareNames
        
    type BoardSquare = Piece option

    type Board =
        {
            squares : BoardSquare array
            halfmove : int
        }
    
    //let emptyBoardSquare : BoardSquare = ( {file = -1; rank = -1}, None )
    
    let toChar pt = 
        match pt with
        | Pawn -> 'P'
        | Knight -> 'N'
        | Bishop -> 'B'
        | Rook -> 'R'
        | Queen -> 'Q'
        | King -> 'K'
    
    let pieceValue pt =
        match pt with
        | Pawn -> 1
        | Knight -> 3
        | Bishop -> 3
        | Rook -> 5
        | Queen -> 9
        | King -> 1000 // well, something massive anyway...
    
    let pieceToString (p : Piece) : string =
        let c = toChar p.pieceType
        if p.colour = White then
            string c
        else
            string <| (c|> System.Char.ToLower)
    
    let boardSquareToString (bsq : BoardSquare) =
        match bsq with
        | Some p -> pieceToString p
        | None -> " "
    
    let printBoardSquare (bsq : BoardSquare) =
        printf "%s" (boardSquareToString bsq)
    
    let printBoard (board : Board) =
        for rank in 7..-1..0 do
            for file in 0..7 do
                printBoardSquare board.squares.[8*rank + file]
                if file = 7 then printf "\n"

    let fromFEN (fen : System.Char) : Piece =
        match fen with
        | 'p' -> { pieceType = Pawn;    colour = Black }
        | 'n' -> { pieceType = Knight;  colour = Black }
        | 'b' -> { pieceType = Bishop;  colour = Black }
        | 'r' -> { pieceType = Rook;    colour = Black }
        | 'q' -> { pieceType = Queen;   colour = Black }
        | 'k' -> { pieceType = King;    colour = Black }
        | 'P' -> { pieceType = Pawn;    colour = White }
        | 'N' -> { pieceType = Knight;  colour = White }
        | 'B' -> { pieceType = Bishop;  colour = White }
        | 'R' -> { pieceType = Rook;    colour = White }
        | 'Q' -> { pieceType = Queen;   colour = White }
        | 'K' -> { pieceType = King;    colour = White }
        | _ -> failwith "Invalid FEN character for piece."
    
    let FENToBoardSquares (char : System.Char) : seq<BoardSquare> =
        match char with
        | char when System.Char.IsDigit(char) ->
            let numEmpty = int <| System.Char.GetNumericValue(char) 
            if (numEmpty > 0) && (numEmpty < 9) then
                seq { for i in 1 .. numEmpty -> None }
            else 
                failwith "Invalid number of empty squares found in FEN."
        | _ ->  seq { yield (Some (fromFEN char)) }
    
    // Create Board from FEN
    let makeBoard (fen:string) =
        // flip ranks as board squares array stores from 1st to 8th rank
        let flipped = fen.Split('/') |> Seq.ofArray |> Seq.rev |> Seq.concat
        {    
            squares = flipped |> Seq.collect FENToBoardSquares |> Seq.toArray
            halfmove = 0
        }

    // Move the piece on src sqaure to dest square.
    //let move (src : string) (dest : string) (board : Board) : Board =
    //    let srcSquare = board.squares.[toIndex src]
    //    let destSquare = board.squares.[toIndex dest]
    //    // TODO match on cases
    //    failwith ""

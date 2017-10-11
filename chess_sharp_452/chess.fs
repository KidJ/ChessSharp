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
            "a1"; "b1"; "c1"; "d1"; "e1"; "f1"; "g1"; "h1";
            "a2"; "b2"; "c2"; "d2"; "e2"; "f2"; "g2"; "h2";
            "a3"; "b3"; "c3"; "d3"; "e3"; "f3"; "g3"; "h3";
            "a4"; "b4"; "c4"; "d4"; "e4"; "f4"; "g4"; "h4";
            "a5"; "b5"; "c5"; "d5"; "e5"; "f5"; "g5"; "h5";
            "a6"; "b6"; "c6"; "d6"; "e6"; "f6"; "g6"; "h6";
            "a7"; "b7"; "c7"; "d7"; "e7"; "f7"; "g7"; "h7";
            "a8"; "b8"; "c8"; "d8"; "e8"; "f8"; "g8"; "h8";
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
    //let move (src : string) (dest : string) (board : Board) : Board option =
    //    let srcSquare = board.squares.[toIndex src]
    //    let destSquare = board.squares.[toIndex dest]
    //    // TODO match on cases
    //    failwith ""

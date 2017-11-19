module Chess

    open log4net

    // todo - core stuff like this in separate modules
    type Int2 =
        {
            x : int
            y : int
        }
        with
        static member (*) (i : Int2, a : int) =
            { x = i.x * a; y = i.y * a }

    let makeInt2 ix iy = { x = ix; y = iy; }

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
        with
        override this.ToString() : string =
            match this with
            | White -> "White"
            | Black -> "Black"

    type Piece =
        {
            pieceType : PieceType
            colour : PieceColour
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
    
    type Square =
        {
            file: int
            rank : int
        }
        with
        static member (+) (left : Square, right : Square) =
            { file = left.file + right.file; rank = left.rank + right.rank }

        static member (+) (sq : Square, i : Int2) = 
            { file = sq.file + i.x; rank = sq.rank + i.y }

        static member flatten f r =
            (r * 8) + f

        static member public toBoardIndex (sq : Square) =
            Square.flatten sq.file sq.rank

        static member fromBoardIndex index = 
            assert ((index > -1) && (index < 64))
            { rank = index / 8; file = index % 8 }

        static member toString (sq : Square) : string =
            squareNames.[Square.toBoardIndex sq]

        static member stringToBoardIndex s =
            let index = Array.tryFindIndex (fun i -> i = s) squareNames
            match index with
            | Some i -> i
            | None -> failwithf "Invalid square index '%s'" s

        static member fromString s =
            s |> Square.stringToBoardIndex |> Square.fromBoardIndex

        static member make (f : int) (r : int) : Square =
            { file = f; rank = r }

    let squareValid (square : Square) =
        square.file >= 0 && square.file <= 7 && square.rank >= 0 && square.rank <= 7      

    type BoardSquare = Piece option

    [<StructuredFormatDisplay("{src}->{dest}")>]
    type Move =
        {
            src : string
            dest : string
        }
        with
        static member tryParse (str : string) : Move option = 
            let strs = str.Split(' ')
            if strs.Length = 2 && (Array.contains strs.[0] squareNames) && (Array.contains strs.[1] squareNames) then
                Some { src = strs.[0]; dest = strs.[1] }
            else
                None

    let containsPieceOfColour (b : BoardSquare) colour = 
        match b with
        | Some p -> p.colour = colour
        | None -> false

    type Board =
        {
            squares : BoardSquare array
            mutable halfmove : int
        }
        with
        member this.getSquare sq =
            this.squares.[Square.toBoardIndex sq]
        member this.setSquare sq bsq =
            this.squares.[Square.toBoardIndex sq] <- bsq
        member this.IsGameOver () = false
        member this.ColourToMove = if this.halfmove % 2 = 0 then PieceColour.White else PieceColour.Black

    let gatherPiecesForColour (board : Board) (colour : PieceColour) =
        board.squares
        |> Array.mapi (fun i bsq -> if (containsPieceOfColour bsq colour) then Some (Square.fromBoardIndex i, bsq.Value) else None)
        |> Array.choose (fun i -> i)
        // better way? - need index though

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

    // TODO - this is actually kinda awkward
    let toFEN (board : Board) : string =
        "I am a board fen, no really"
    //    // need logic for taking multiple sequential empty squares and concatting them
    //    // for each rank
    //    // create the string for the rank
    //    // add string ""
    //    // if <> rank 0 add "/"
    
    //let toString (Move (src,dest)) = src + "->" + dest

    let isLegal (board : Board) (move : Move) : bool =
        failwith ""
        // does player have a piece on src square?
        // can the player move this piece to the dest square?
        //  is it empty or is an opponent piece on it

    let makeMove (board : Board) (move : Move) : Board =
        failwith ""
    
    let move (board : Board) (move : Move) : Board option =
        // is the move legal for this board?
        if isLegal board move then
            Some (makeMove board move)
        else
            failwithf "Move %A not legal for passed board %s" move (toFEN board)
            None

    let printBoardSquare (bsq : BoardSquare) =
        printf "%s" (boardSquareToString bsq)
    
    let printBoard (board : Board) =
        printf "--------\n"
        for rank in 7..-1..0 do
            for file in 0..7 do
                printBoardSquare board.squares.[Square.flatten file rank]
                if file = 7 then printf "\n"
        printf "--------\n"
    
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
    
    let turnColour (board : Board) : PieceColour = 
        if board.halfmove % 2 = 0 then
            White
        else
            Black

    // pawn, rook, bishop, queen and king moves (excluding castling, and capture for the king where he can be recaptured (ie in check))
    // can share the same logic - f : inputSquare: Square, direction : (int*int), maxSpaces : int, testCheck : bool -> Square list
    // only requires parameterising the squares, max squares moved and whether check allowed
    // maybe with the king / check test we special case this anyway...
    // Piece generation functions assume empty board, this way they can be either used to generate look up tables which are filtered on board state,  or called directly.
    
    // can a piece of this colour move to this square?
    // note that this deliberately ignores origin square, it just looks at whether the square is valid and unoccupied by piece of same colour.
    let canMoveTo (board : Board) (colour : PieceColour) (sq : Square) : bool =
        if squareValid sq then
            match (board.getSquare sq) with
            | Some p -> p.colour <> colour
            | None ->  true
        else 
            false

    // get all valid moves on the board in the passed direction
    let movesForDirection (board : Board) (colour : PieceColour) (sq: Square) (maxDist : int) (dir : Int2) : Square list =    
        seq { 1 .. maxDist }
        |> Seq.map (fun i -> sq + (Square.make (dir.x * i) (dir.y * i)))
        |> Seq.takeWhile (canMoveTo board colour)
        |> List.ofSeq

    let whitePawnForwardMoves board sq = 
        match sq.rank with
        | 1 -> movesForDirection board White sq 2 (makeInt2 0 1)
        | 7 -> [] // this is actually invalid as you would be promoted
        | _ -> movesForDirection board White sq 1 (makeInt2 0 1)

    let whitePawnDiagonalMoves (board : Board) (sq : Square) =
        [ sq + (Square.make 1 1); sq + (Square.make -1 1) ]
        |> List.filter (fun dest -> ((board.getSquare dest).IsSome && ((board.getSquare dest).Value.colour = Black)))
    
    let whitePawnEnPassantMoves board sq =
        [] // TODO

    let whitePawnMoves board sq = 
        List.concat [ whitePawnForwardMoves board sq; whitePawnDiagonalMoves board sq; whitePawnEnPassantMoves board sq ]
    
    let blackPawnForwardMoves board sq = 
        match sq.rank with
        | 6 -> movesForDirection board Black sq 2 (makeInt2 0 -1)
        | 0 -> [] // this is actually invalid as you would be promoted
        | _ -> movesForDirection board Black sq 1 (makeInt2 0 -1)

    let blackPawnDiagonalMoves (board : Board) (sq : Square) =
        [ sq + (Square.make 1 -1); sq + (Square.make -1 -1) ]
        |> List.filter (fun dest -> ((board.getSquare dest).IsSome && ((board.getSquare dest).Value.colour = White)))
    
    let blackPawnEnPassantMoves board sq =
        [] // TODO

    let blackPawnMoves board sq = 
        List.concat [ blackPawnForwardMoves board sq; blackPawnDiagonalMoves board sq; blackPawnEnPassantMoves board sq ]
    
    let pawnMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        match colour with
        | White -> whitePawnMoves board sq
        | Black -> blackPawnMoves board sq

    let knightMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        [ 
            sq + (Square.make 1 2)
            sq + (Square.make 2 1)
            sq + (Square.make 2 -1)
            sq + (Square.make 1 -2)
            sq + (Square.make -1 -2)
            sq + (Square.make -2 -1)
            sq + (Square.make -2 1)
            sq + (Square.make -1 2)
        ] |> List.filter (canMoveTo board colour)
    
    // Always unable to move exactly 8 in any distance, so naturally forms valid termination condition.
    let bishopMoves (board : Board) (colour : PieceColour) (sq : Square) =
        [ makeInt2 1 1; makeInt2 1 -1; makeInt2 -1 -1; makeInt2 -1 1]
        |> List.map (movesForDirection board colour sq 8)
        |> List.concat
    
    let rookMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        [ makeInt2 1 0; makeInt2 0 -1; makeInt2 -1 0; makeInt2 0 1]
        |> List.map (movesForDirection board colour sq 8)
        |> List.concat

    let queenMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        [ makeInt2 1 0; makeInt2 0 -1; makeInt2 -1 0; makeInt2 0 1; makeInt2 1 1; makeInt2 1 -1; makeInt2 -1 -1; makeInt2 -1 1]
        |> List.map (movesForDirection board colour sq 8)
        |> List.concat
    
    // King can only move 1 space
    // TODO - further filter list for positions where the king isn't in check
    // check test logic - is there any move where the king is taken?
    let kingMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        [ makeInt2 1 0; makeInt2 0 -1; makeInt2 -1 0; makeInt2 0 1; makeInt2 1 1; makeInt2 1 -1; makeInt2 -1 -1; makeInt2 -1 1]
        |> List.map (movesForDirection board colour sq 1)
        |> List.concat
    
    // Generate legal moves for a given piece on the passed board.
    let generateMovesForPiece (board : Board) (piece : Piece) (sq : Square) : Square list =
        match piece.pieceType with
        | Pawn -> pawnMoves board piece.colour sq
        | Knight -> knightMoves board piece.colour sq
        | Bishop -> bishopMoves board piece.colour sq
        | Rook -> rookMoves board piece.colour sq
        | Queen -> queenMoves board piece.colour sq
        | King -> kingMoves board piece.colour sq

    // generate valid moves for all pieces
    let generateValidMoves (board : Board) : Move [] =
        let moveColour = turnColour board
        let pieces = gatherPiecesForColour board moveColour
        let validMoves = pieces |> Array.map (fun (sq,p) -> (generateMovesForPiece board p sq) |> List.map (fun sqDest -> {src = (Square.toString sq); dest = (Square.toString sqDest)}) |> List.toArray)
        validMoves |> Array.concat
    
    // Move the piece on src square to dest square
    // requires:
    // - source and dest squares to be valid pieces
    // - source square to have a piece belonging to current player
    // - dest square to either be empty or contain other players piece
    let tryMove (board : Board) (move : Move) : bool =
        let srcSquare = Square.fromString move.src
        let destSquare = Square.fromString move.dest

        let srcBoardSquare = board.getSquare srcSquare
        let destBoardSquare = board.getSquare destSquare

        let moveColour = turnColour board

        let prevMove = board.halfmove
        if srcBoardSquare.IsSome then
            if srcBoardSquare.Value.colour = moveColour then
                let squares = generateMovesForPiece board srcBoardSquare.Value srcSquare
                if List.contains destSquare squares then
                    board.setSquare destSquare srcBoardSquare
                    board.setSquare srcSquare None
                    board.halfmove <- board.halfmove + 1
        
        board.halfmove > prevMove
    
    
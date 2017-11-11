﻿module Chess

    // todo - core stuff in separate modules
    type Int2 =
        {
            x : int
            y : int
        }
        with
        static member (*) (i : Int2, a : int) =
            { x = i.x * a; y = i.y * a }

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
        with
        static member (+) (left : Square, right : Square) =
            { file = left.file + right.file; rank = left.rank + right.rank }
        static member (+) (sq : Square, i : Int2) = 
            { file = sq.file + i.x; rank = sq.rank + i.y }
    
    let makeSquare (f : int) (r : int) : Square = { file = f; rank = r }

    let onBoard (square : Square) =
        square.file >= 0 && square.file <= 7 && square.rank >= 0 && square.rank <= 7
   
    //let NumSquares = 64
    
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

    // unflatten index to Square
    //let unflatten index : Square =
    //    assert ((index > -1) && (index < 64))
    //    { rank = index / 8; file = index % 8 }

    //let toString bsq = squareNames.[flatten bsq]

    // slooooow... and now repeating logic from Square -> flattened index
    let toIndex (squareName : string) =
        Array.tryFindIndex (fun s -> s = squareName) squareNames
        
    type BoardSquare = Piece option

    type Board =
        {
            squares : BoardSquare array
            mutable halfmove : int
        }
        with
        static member private flatten rank file = (rank * 8) + file
        static member public boardIndex sq = Board.flatten sq.rank sq.file
        member this.getSquare sq = this.squares.[Board.boardIndex sq]
    
    type Move = Move of string * string // (src,dest) squares
    type MoveHistory = MoveHistory of Move list

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

    //    do
    //        let mutable i = 0
    //        for rank in 7..-1..0 do
                  

    //            for file in 0..7 do
    //                str.[i] <- (boardSquareToString board.squares.[flatten rank file])
    //                if (file = 7 && not (file <> 0)) then str += "/"
    //                i <- i + 1
    
    let toString (Move (src,dest)) = src + "->" + dest

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
            failwithf "Move %s not legal for passed board %s" (toString move) (toFEN board)
            None

    let printBoardSquare (bsq : BoardSquare) =
        printf "%s" (boardSquareToString bsq)
    
    let printBoard (board : Board) =
        printf "--------\n"
        for rank in 7..-1..0 do
            for file in 0..7 do
                printBoardSquare board.squares.[flatten rank file]
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

    // Move the piece on src square to dest square
    // requires:
    // - source and dest squares to be valid pieces
    // - source square to have a piece belonging to current player
    // - dest square to either be empty or contain other players piece
    let tryMove (src : string) (dest : string) (board : Board) : unit =
        let srcIndex = toIndex src
        let destIndex = toIndex dest
        match (srcIndex, destIndex) with
        | (None,_) | (_,None) | (None,None) -> failwithf "invalid square name(s) passed %s %s" src dest
        | _ -> 
            let srcSquare = board.squares.[srcIndex.Value]
            let destSquare = board.squares.[destIndex.Value]
            let moveColour = turnColour board
            if srcSquare.IsSome && srcSquare.Value.colour = moveColour then
                if not (destSquare.IsSome && (destSquare.Value.colour = moveColour)) then
                    board.squares.[destIndex.Value] <- srcSquare
                    board.squares.[srcIndex.Value] <- None
                    printf "Move succeeded %s -> %s\n" src dest
                    board.halfmove <- board.halfmove + 1
                else
                    failwith "Destination square contains piece of current turn colour"
            else
                failwith "Piece at source square not valid for passed move"
    
    // Generate list of all possible moves for the given board
    //let generatePossibleMoves (board : Board) : Move [] =
        //[ ]
        // for each piece of current colour
        //     for each board square, if is Some p and has colour = current colour
        //          generateMovesForPiece p
        //          filter any moves which
        //          map sequence to generate (src * dest) move strings
    
    // Piece generation functions assume empty board, this way they can be either used to generate look up tables which are filtered on board state,  or called directly.
    let pawnMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        match colour with
        | White -> 
            match sq.rank with
            | 2 -> [ sq + (makeSquare 0 1); sq + (makeSquare 0 2) ]
            | 8 -> []
            | _ -> [ sq + (makeSquare 0 1) ]
        | Black ->            
            match sq.rank with
            | 7 -> [ sq + (makeSquare 0 -1); sq + (makeSquare 0 -2)  ]
            | 1 -> []
            | _ -> [ sq + (makeSquare 0 -1) ]
        // TODO - en passant...
    
    let knightMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        [ 
            sq + (makeSquare 1 2)
            sq + (makeSquare 2 1)
            sq + (makeSquare 2 -1)
            sq + (makeSquare 1 -2)
            sq + (makeSquare -1 -2)
            sq + (makeSquare -2 -1)
            sq + (makeSquare -2 1)
            sq + (makeSquare -1 2)
        ] |> List.filter onBoard


        // rook, bishop, queen and king moves (excluding castling, and capture for the king where he can be recaptured (ie in check))
        // can share the same logic - f : inputSquare: Square, direction : (int*int), maxSpaces : int, testCheck : bool -> Square list
        // only requires parameterising the squares, max squares moved and whether check allowed
        // maybe with the king / check test we special case this anyway...

    // get all valid moves 
    let movesForDirection (board : Board) (sq: Square) (colour : PieceColour) (dir : Int2) (maxDist : int) : Square list =    
        let moves = seq { 0 .. 7 }

        //^^ how about instead...
        // use tryFind to get point at which we should terminate our search
        // then can just [ 0 .. n ] |> List.map to generate squares for moves?

        /// arf...
        let moves = []
        let mutable loop = true
        let mutable i = 0
        while loop do
            // get our square to test
            i <- i + 1
            let diri = dir * i
            let sqTest = sq + (makeSquare diri.x diri.y)
            if onBoard sqTest then
                let boardSquare = (board.getSquare sqTest)
                match boardSquare with
                | Some piece ->
                    if piece.colour = colour then
                        loop <- false
                    else
                        moves
                | None -> 
                    moves = sqTest :: moves
            else
                break

            

            let 

            List.append newsq moves
                
        
        let genDiagonal (x,y) : Square list =
            failwithf ""

        let bishopDirections = [ (1, 1); (1,-1); (-1,-1); (-1, 1) ]

        bishopDirections
        |> List.map movesForDirection board 
        //|> ...

    let bishopMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        [ makeSquare 0 1 ]
    
    let rookMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        [ makeSquare 0 1 ]

    let queenMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        [ makeSquare 0 1 ]
    
    let kingMoves (board : Board) (colour : PieceColour) (sq : Square) : Square list =
        [ makeSquare 0 1 ]
    
    // given a square and a piece (type & colour), what squares can it move to?
    let generateMovesForPiece (board : Board) (piece : Piece) (sq : Square) : Square list =
        match piece.pieceType with
        | Pawn -> pawnMoves board piece.colour sq
        | Knight -> knightMoves board piece.colour sq
        | Bishop -> bishopMoves board piece.colour sq
        | Rook -> rookMoves board piece.colour sq
        | Queen -> queenMoves board piece.colour sq
        | King -> kingMoves board piece.colour sq
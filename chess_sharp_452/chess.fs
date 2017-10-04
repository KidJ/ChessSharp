module chess

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
            rank : int;
        }

    type BoardSquare = Square * (Piece option)

    type Board =
        {
            squares : BoardSquare list;
            halfmove : int;
        }
        with
        member this.findSquare f r = 
            List.find (fun (s,_) -> s = {rank = r; file = f}) this.squares

    //let fromChar (c : char) : Piece = 
    //    failwith ""

    let toChar (pt : PieceType) =
        match pt with
        | Pawn -> 'P'
        | Knight -> 'N'
        | Bishop -> 'B'
        | Rook -> 'R'
        | Queen -> 'q'
        | King -> 'K'

     let value (pt : PieceType) : int =
        match pt with
        | Pawn -> 1
        | Knight -> 3
        | Bishop -> 3
        | Rook -> 5
        | Queen -> 9
        | King -> 1000 // well something massive anyway...

    let toString (p : Piece) : string =
        let c = toChar p.pieceType
        if p.colour = White
            then string c
        else c|> System.Char.ToLower |> string
    
    //let invalidSquare = { rank = -1; file = -1 }
    
    let displaySquare (square : BoardSquare) =
        match snd square with
        | Some p -> printf "%A" p.fen
        | None -> printf " "
        // hmmmm
        if (fst square).rank = 1 then
            printf "\n"

    let printBoard (board : Board) =
        List.iter displaySquare board.squares

    let fenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" // w KQkq - 0 1"
    let fenEmpty = "8/8/8/8/8/8/8/8"

    let FENCharIsPiece (c : System.Char) =
        match c with
        | 'r' | 'n' | 'b' | 'q' | 'k' | 'p' | 'R' | 'N' | 'B' | 'Q' | 'K' | 'P' -> true
        | _ -> false
    
    // ! TODO - use Seq.collect!
    let FENCharToBoardSquares (char : System.Char) : seq<BoardSquare> =
        match char with
        | char when FENCharIsPiece char -> Seq.init 1 (fun i -> invalidSquare, Some {fen = char})
        | char when System.Char.IsDigit(char) ->
            let numempty = int <| System.Char.GetNumericValue(char) 
            seq {
                for i in 1..numempty -> (invalidSquare, None)
            }
        | _ -> failwith "Invalid char found in FEN."

    let makeRank (fenRank : string) (r : int) : BoardSquare seq =
        seq {
            for char in fenRank do
                yield! FENCharToBoardSquares char
        } 
        |> Seq.mapi (fun i (sq,p) -> ({rank = r; file = i}, p) ) // number squares         
    
    let makeBoard (fen : string) : Board =
        let ranks = fen.Split '/' |> Array.toList
        if ranks.Length = 8 then
            let sqList = seq { for i in 8 .. 1 do yield! makeRank ranks.[i] i } |> Seq.toList
            { squares  = sqList; halfmove = 0 }
        else
            failwith "Invalid FEN."
    


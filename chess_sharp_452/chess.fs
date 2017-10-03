module chess

    type Piece =
        { fen : System.Char } // FEN notation

    // rank 1->8, file 1->8 corresponds to a->h
    type Square = {
        rank : int;
        file: int
    }

    let invalidSquare = { rank = -1; file = -1 }

    type BoardSquare = Square * (Piece option)

    // Chess board state
    type Board =
        { squares : BoardSquare list; halfmove : int; }
    
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
    


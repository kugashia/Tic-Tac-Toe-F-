namespace QUT
    
    module FSharpImpureTicTacToeModel =

        open System.Collections.Generic
        open System
        
        type Player = Nought | Cross (* implement type *)

        (*
        * toggle: flip the turn of the current player and assign the value at the same time.
                  Used to reduce the amount of repetitive code,
                  Used in ApplyMove and GameOutcome
        *)
        let toggle = function (* pattern matching input *)
            | Cross -> (Nought, "X")
            | Nought -> (Cross, "O")

        type GameState =        
            { 
                mutable turn: Player (* implement type *) 
                mutable size: int
                mutable pieces: Dictionary<int*int, string> (* Dictionary is mutable as mention ind F# Expert book *)
            } 
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = this.pieces.Item(row, col)

        type Move = 
            { 
                mutable row: int (* implement type *)
                mutable column: int
            }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.column
        
        let Lines (size:int) : seq<seq<int*int>> = 
            seq { 
                for x in 0 .. size-1 do 
                    yield seq [for y in 0 .. size-1 do yield (x,y)] 
                    yield seq [for y in 0 .. size-1 do yield (y,x)]

                yield seq [for y in 0 .. size-1 -> (y,y)]
                yield seq [for y in 0 .. size-1 -> (y,(size-1)-y)]
            }

        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 

            // lineValues: get the values corresponding tp each piece -> [X,O,X]
            let lineValues = line |> Seq.map (fun piece -> game.pieces.Item(fst piece, snd piece))
            let contain x = Seq.contains x lineValues
            let AllValuesEqual player = lineValues |> Seq.forall ((=) player)

            // if statement used since the expression changes
            if (contain "X" && contain "O") then Draw
            elif (AllValuesEqual "X") then Win (Player.Cross, line)
            elif (AllValuesEqual "O") then Win (Player.Nought, line)      
            else Undecided

        let GameOutcome (game:GameState) = 

            let lines = Lines game.size
            // get the outcome of each line, and keep tracking of the line by returning a tuple of (outcome, line)
            let sequencOfOutcome = lines |> Seq.map (fun line -> ((CheckLine game line), line))
            let allLinesDraw = sequencOfOutcome |> Seq.forall (fun (outcome,_) -> outcome = Draw)
            let winningLine = sequencOfOutcome |> Seq.tryFind (fun (outcome,line) -> outcome = Win (fst (toggle game.turn), line))

            if (allLinesDraw) then Draw
            elif (winningLine.IsSome) then fst (winningLine).Value
            else Undecided
                 
        let ApplyMove game (move:Move) =
            game.pieces.[(move.row, move.column)] <- snd (toggle game.turn)
            game.turn <- fst (toggle game.turn)
            game

        let CreateMove row col = {Move.row=row; Move.column=col}

        let GameStart first size = 
            // initialize the pieces to be empty string
            let pieces = new Dictionary<int*int, string>()
            for x in 0 .. size-1 do for y in 0 .. size-1 do pieces.Add((x,y), "")
            {
                GameState.turn = first
                GameState.size = size
                GameState.pieces = pieces
            }

        // plus other helper functions ...
        let getTurn (game:GameState) = game.turn

        let heuristic (game:GameState) (player:Player) = 
            match GameOutcome game with
            | Win (p, _) when p = player -> 1
            | Draw -> 0
            | _ -> -1

        let gameOver (game:GameState) = 
            match GameOutcome game with
            | Win (_, _)  -> true
            | Draw -> true
            | _ -> false

        (*
        * moveGenerator: return a list of available moves by looping through the Dictionery,
                         Also apply CreateMove function for each empty string, and add the 
                         empty string to the resizable array (As used in F# Expert Book).
        *)
        let moveGenerator (game:GameState) = 
            let availableMoves = ResizeArray<Move>()
            for kvp in game.pieces do
                if (kvp.Value = "") then availableMoves.Add(CreateMove (fst kvp.Key) (snd kvp.Key))
            Seq.toList availableMoves

        (*
        * UndoMove: Remove the applied move in MiniMaxAlphaBeta and Flip the turn.
        *)
        let UndoMove game move = 
            game.pieces.[(move.row, move.column)] <- ""
            game.turn <- fst (toggle game.turn)

        (*
        * MiniMaxAlphaBeta: Use the same structure used in C#. The while loop acts as a for loop,
                            and the break statement in the expression of the while loop
                            (mutableAlpha < mutableBeta or reach the end of the loop)
        *)
        let rec MiniMaxAlphaBeta alpha beta game maximizingPlayer: Option<Move> * int = 

            NodeCounter.Increment()

            let availableMoves = moveGenerator game
            let mutable bestMove = 
                if (maximizingPlayer) then Tuple.Create(None, -1000)
                else Tuple.Create(None, +1000)

            if (gameOver game || availableMoves.IsEmpty) then (None, heuristic game Cross)
            else 

                let mutable mutableAlpha = alpha
                let mutable mutableBeta = beta
                let mutable i = 0

                while (mutableAlpha < mutableBeta && i < availableMoves.Length) do
                        
                    let move, score = MiniMaxAlphaBeta mutableAlpha mutableBeta (ApplyMove game availableMoves.[i]) (not maximizingPlayer)
                    UndoMove game availableMoves.[i]

                    if (maximizingPlayer) then 
                        if (score > snd bestMove) then 
                            bestMove <- (Some availableMoves.[i], score)
                            mutableAlpha <- max mutableAlpha score
                    else 
                        if (score < snd bestMove) then 
                            bestMove <- (Some availableMoves.[i], score)
                            mutableBeta <- min mutableBeta score
                    i <- i + 1

                bestMove

        let FindBestMove game =          
            NodeCounter.Reset()
            if (game.turn = Cross) then
                let move, score = MiniMaxAlphaBeta -1 1 game true
                move.Value
            else
                let move, score = MiniMaxAlphaBeta -1 1 game false
                move.Value
            
        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game move
                member this.FindBestMove(game)           = FindBestMove game
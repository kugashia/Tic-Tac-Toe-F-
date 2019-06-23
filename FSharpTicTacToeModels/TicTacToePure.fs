namespace QUT

    module FSharpPureTicTacToeModel =
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        (*
        * toggle: flip the turn of the current player and assign the value at the same time.
                  Used to reduce the amount of repetitive code,
                  Used in ApplyMove and GameOutcome
        *)
        let toggle = function (* pattern matching input *)
                | Cross -> (Nought, "X")
                | Nought -> (Cross, "O")

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            {
                row: int (* TODO implement type *) 
                column: int
            }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.column

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { 
                turn: Player (* TODO implement type *) 
                size: int
                pieces: Map<int*int, string> (* immutable, as mentioned in Microsoft F# Docs*)
            }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = this.pieces.Item (row, col)

        let CreateMove row col = {Move.row=row; Move.column=col}

        let ApplyMove (oldState:GameState) (move: Move) = 
            {
                GameState.turn = fst (toggle oldState.turn)
                GameState.pieces = Map.map (fun key value -> if key = (move.row, move.column) then snd (toggle oldState.turn) else value) oldState.pieces
                GameState.size = oldState.size
            }

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
        let Lines (size:int) : seq<seq<int*int>> = 
            seq { 
                for x in 0 .. size-1 do 
                    yield seq [for y in 0 .. size-1 do yield (x,y)] 
                    yield seq [for y in 0 .. size-1 do yield (y,x)]

                yield seq [for y in 0 .. size-1 -> (y,y)]
                yield seq [for y in 0 .. size-1 -> (y,(size-1)-y)]
            }

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 

            // lineValues: get the values corresponding tp each piece -> [X,O,X]
            let lineValues = line |> Seq.map (fun piece -> game.pieces.Item piece)
            let contain x = Seq.contains x lineValues
            let AllValuesEqual player = lineValues |> Seq.forall ((=) player)

            // if statement used since the expression changes
            if (contain "X" && contain "O") then Draw
            elif (AllValuesEqual "X") then Win (Player.Cross, line)
            elif (AllValuesEqual "O") then Win (Player.Nought, line)      
            else Undecided

        let GameOutcome (game:GameState) = 

            let lines = Lines game.size
            // sequencOfOutcome: get the outcome of each line, and keep tracking of the line by returning a tuple of (outcome, line)
            let sequencOfOutcome = lines |> Seq.map (fun line -> ((CheckLine game line), line))
            let allLinesDraw = sequencOfOutcome |> Seq.forall (fun (outcome,_) -> outcome = Draw)
            let winningLine = sequencOfOutcome |> Seq.tryFind (fun (outcome,line) -> outcome = Win (fst (toggle game.turn), line))

            if (winningLine.IsSome) then fst (winningLine).Value
            elif (allLinesDraw) then Draw
            else Undecided

        let GameStart (firstPlayer:Player) size =        
            {
                GameState.turn = firstPlayer
                GameState.size = size
                GameState.pieces = Map [ for x in 0 .. size-1 do for y in 0 .. size-1 -> ((x,y), "")]
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
        * moveGenerator: return all the available moves (empty strings) by filtering the Map
                         , convert the map to sequence, then apply createMove function to each pieces
        *)
        let moveGenerator (game:GameState) = 
            game.pieces
            |> Map.filter (fun _ value -> value = "") 
            |> Map.toSeq
            |> Seq.map (fun (cell,_) -> {Move.row=fst cell; Move.column=snd cell})

        let MiniMax game = 
            let move, score = (GameTheory.MiniMaxGenerator heuristic getTurn gameOver moveGenerator ApplyMove) game game.turn
            move.Value

        let MiniMaxWithPruning game = 
            let move, score = (GameTheory.MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver moveGenerator ApplyMove) -1 1 game game.turn
            move.Value

        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) =  (MiniMax game)


        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = (MiniMaxWithPruning game)
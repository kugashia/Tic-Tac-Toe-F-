namespace QUT

    module GameTheory =   

        type Move = Game | Branch of Move seq

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax game perspective =

                NodeCounter.Increment() 
                
                let currentPlayer = getTurn game
                let moves = moveGenerator game

                if (gameOver game || Seq.isEmpty moves) then (None, heuristic game perspective)
                else 
                    
                    (*
                        keep tracking the index of each move and the score. and return the index of the 
                        best move with highest score 
                    *)
                    let moveIndxAndScore = 
                        let minimaxMovesResult = moves |> Seq.mapi (fun i move -> i, snd (MiniMax (applyMove game move) perspective))
                        if (perspective = currentPlayer) then minimaxMovesResult |> Seq.maxBy snd  
                        else minimaxMovesResult |> Seq.minBy snd

                    let bestMove = Seq.tryItem (fst moveIndxAndScore) moves
                    (bestMove, snd moveIndxAndScore)          
                    
            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                
                NodeCounter.Increment()

                let moves = moveGenerator oldState |> Seq.toList
                let initalMove = 
                    if (perspective = getTurn oldState) then (Option<'Move>.None, -1000)
                    else (Option<'Move>.None, +1000)

                if (gameOver oldState || moves.IsEmpty) then (Option<'Move>.None, heuristic oldState perspective)
                else                     
                    
                    (*
                        Compare the current move returned from MiniMax to the previous 
                        best move (old best Move) by combining both of them in a sequence, 
                        then tacking the max or min based on the current turn. 

                        if alpha larger than beta, then return the best move (break)
                        else keep calling the Higher order function recursively with 
                        the remaining moves (the tail)
                    *)
                    let rec FindBestMove availableMoves alpha beta game perspective (oldMove: Option<'Move> * int) =
                        
                        match availableMoves with 
                        | [] -> oldMove
                        | firstMove :: remainingMoves ->

                            let newMove = MiniMax alpha beta (applyMove oldState firstMove) perspective     

                            if (perspective = (getTurn game)) then
                                let bestMove = (seq [oldMove; (Some firstMove, snd newMove)]) |> Seq.maxBy snd
                                let newAlpha = max alpha (snd bestMove)
                                if (newAlpha >= beta) then bestMove
                                else FindBestMove remainingMoves newAlpha beta game perspective bestMove
                            else            
                                let bestMove = (seq [oldMove; (Some firstMove, snd newMove)]) |> Seq.minBy snd
                                let newBeta = min beta (snd bestMove)
                                if (alpha >= newBeta) then bestMove
                                else FindBestMove remainingMoves alpha newBeta game perspective bestMove

                    FindBestMove moves alpha beta oldState perspective initalMove

            NodeCounter.Reset()
            MiniMax
using System;
using System.Collections.Generic;
using System.Linq;

namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.Cross;
        public Player Nought => Player.Nought;

        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }

        /*
        * ApplyMove: Use the SetPiece method in the Game instance to set the piece and flip the turn
        * @ params: 
        * game: Game
        * return: the Game with the applied move
        */
        public Game ApplyMove(Game game, Move move)
        {
            game.SetPiece(move.Row, move.Col, game.Turn == Nought ? "O" : "X");
            game.Turn = (game.Turn == Nought ? Cross : Nought);
            return game;
        }

        public Move CreateMove(int row, int col) => new Move(row, col);

        public Move FindBestMove(Game game)
        {
            NodeCounter.Reset();
            return MiniMaxAlphaBeta(-1, 1, game, game.Turn == Cross ? true : false).Item1;
        }

        /*
        * MiniMaxAlphaBeta: Compute the alpha beta prunning version of MiniMax algorithm
        * params:
        * @ Alpha : int
        * @ beta: int
        * @ game : Game
        * @ maximizingPlayer : bool 
        * return: best move
        */
        public Tuple<Move, int> MiniMaxAlphaBeta(int alpha, int beta, Game game, bool maximizingPlayer)
        {

            NodeCounter.Increment();

            Tuple<Move, int> bestMove = Tuple.Create(CreateMove(-1, -1), Heuristic(game, Player.Cross));

            if (GameOver(game))
            {
                return bestMove;
            }

            int bestVal = maximizingPlayer ? int.MinValue : int.MaxValue;
            List<Move> availableMoves = MoveGenerator(game);

            foreach (Move move in availableMoves)
            {
                int score = MiniMaxAlphaBeta(alpha, beta, ApplyMove(game, move), !maximizingPlayer).Item2;
                UndoMove(game, move);

                if (maximizingPlayer)
                {
                    if (score > bestVal)
                    {
                        bestVal = score;
                        bestMove = Tuple.Create(move, score);
                        alpha = Math.Max(alpha, bestVal);
                    }
                }
                else
                {
                    if (score < bestVal)
                    {
                        bestVal = score;
                        bestMove = Tuple.Create(move, score);
                        beta = Math.Min(beta, bestVal);
                    }
                }
                if (alpha >= beta) break;
            }
            return bestMove;
        }

        /*
        * MoveGenerator: find the empty pieces, apply createMove function to each piece 
        *                , and return a list of available moves
        * params:
        * @ game : Game
        * return: list of moves, List<Move>
        */
        public List<Move> MoveGenerator(Game game)
        {
            var availableMoves = game.GetAllPieces().Where(piece => piece.Value == "");
            return availableMoves.Select(piece => CreateMove(piece.Key.Item1, piece.Key.Item2)).ToList();
        }

        /*
        * Won: Check to see if a player won, if so => return the winning line; else return empty list.
        * params:
        * @ game : Game
        * @ player : the string value of the player
        * return: winning line if available, List<Tuple<int, int>>
        */
        public List<Tuple<int, int>> Won(Game game, string player)
        {
            var winningLine = game.GetWinningLines().Find(line => line.All(cell => game.getPiece(cell.Item1, cell.Item2) == player));
            if (winningLine != null) return winningLine;
            return new List<Tuple<int, int>>{ };
        }

        /*
        * UndoMove: remove the move, and flip the turn to match the old game state, this function used in MiniMaxAlphaBeta
        * params:
        * @ game : Game
        * @ move : the string value of the player
        * return: void
        */
        public void UndoMove(Game game, Move move)
        {
            game.SetPiece(move.Row, move.Col, "");
            game.Turn = (game.Turn == Nought ? Cross : Nought);
        }

        /*
        * Draw: check if all winning lines have any X's or any O's return true  
        * params:
        * @ game : Game
        * return: bool
        */
        public bool Draw(Game game)
        {
            return game.GetWinningLines().All(line => line.Any(c => game.getPiece(c.Item1, c.Item2) == "X") && line.Any(c => game.getPiece(c.Item1, c.Item2) == "O"));
        }

        /*
        * GameOver: check if the game is draw or any player won, then the game is over
        * params:
        * @ game : Game
        * return: bool
        */
        public bool GameOver(Game game)
        {
            if (Draw(game) || Won(game, "X").Any() || Won(game, "O").Any()) return true;
            return false;                
        }

        /*
        * Heuristic: return the score of the player, -1, 0, or 1
        * params:
        * @ game : Game
        * @ player : Player
        * return: int
        */
        public int Heuristic(Game game, Player player)
        {
            if (Won(game, Player.Cross == player ? "X" : "O").Any()) return 1;
            else if (Draw(game)) return 0;
            else return -1;
        }

        /*
        * GameOutcome: return the outcome of the game, in Winning case, the NewWin function is called
        *              to create a win case
        * params:
        * @ game : Game
        * return: TicTacToeOutcome<Player>
        */
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            if (Won(game, "X").Any()) return TicTacToeOutcome<Player>.NewWin(Cross, Won(game, "X"));
            else if (Won(game, "O").Any()) return TicTacToeOutcome<Player>.NewWin(Nought, Won(game, "O"));
            else if (Draw(game)) return TicTacToeOutcome<Player>.Draw;
            else return TicTacToeOutcome<Player>.Undecided;
        }

        public Game GameStart(Player first, int size) => new Game(first, size);
    }
}
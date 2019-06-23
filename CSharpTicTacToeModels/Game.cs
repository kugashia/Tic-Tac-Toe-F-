using System;
using System.Collections.Generic;

namespace QUT.CSharpTicTacToe
{

    public class Game : ITicTacToeGame<Player>
    {

        private Dictionary<(int, int), string> pieces;
        private List<List<Tuple<int, int>>> winningLines;

        public Game(Player player, int gameSize)
        {
            Turn = player;
            Size = gameSize;
            pieces = new Dictionary<(int, int), string>();
            winningLines = new List<List<Tuple<int, int>>>();
            InitializeWinningLines();
        }

        /*
        * InitializeWinningLines: create winning Lines for NxN grid, once every time a game is created 
        *                         , also initialize the pieces to be empty
        * return: void
        */
        private void InitializeWinningLines()
        {
            var D1Line = new List<Tuple<int, int>>();
            var D2Line = new List<Tuple<int, int>>();
            for (int x = 0; x < Size; x++)
            {
                var HorizontalLine = new List<Tuple<int, int>>();
                var VerticalLine = new List<Tuple<int, int>>();
                for (int y = 0; y < Size; y++)
                {
                    HorizontalLine.Add(Tuple.Create(x, y));
                    VerticalLine.Add(Tuple.Create(y, x));
                    pieces[(x, y)] = "";
                }
                D1Line.Add(Tuple.Create(x, x));
                D2Line.Add(Tuple.Create(x, Size - x - 1));
                winningLines.Add(HorizontalLine);
                winningLines.Add(VerticalLine);
            }
            winningLines.Add(D1Line);
            winningLines.Add(D2Line);
        }

        public int Size { get; }
        public Player Turn { get; set; }
        public string getPiece(int row, int col) => pieces.TryGetValue((row, col), out var player) ? player : "";
        public void SetPiece(int row, int col, string player) => pieces[(row, col)] = player;
        public List<List<Tuple<int, int>>> GetWinningLines() => winningLines;
        public Dictionary<(int, int), string> GetAllPieces() => pieces;
    }
}
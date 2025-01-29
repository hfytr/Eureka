module Main where

import Board
import Control.Monad.ST (runST)
import Data.Massiv.Array
import Data.Maybe (fromJust)
import Debug.Trace

startFen :: String
startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

main :: IO ()
main =
  print $
    runST $ do
      board <- fromJust <$> parseFen "rnbqkbnr/ppp1p1pp/B7/3p1p2/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 1 3"
      (moves, movesLen) <- genMoves board
      makeMove board $ newMove 4 6 Rook Castle
      unmakeMove board

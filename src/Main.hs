module Main where

import Board
import Data.Maybe (fromJust)

main :: IO ()
main = do
  let board = fromJust $ parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      moves = genMoves board
  print moves

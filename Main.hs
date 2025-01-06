module Main where

import Board qualified as B
import Data.Maybe (fromJust)

main :: IO ()
main = do
  let board = fromJust $ B.parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  print $ B.genMoves board

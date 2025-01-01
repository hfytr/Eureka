module Main where

import Board
import Data.Maybe (fromJust)

main :: IO ()
main = do
  print $ bitbs $ fromJust $ parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

{-# LANGUAGE TypeFamilies #-}

module Board where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Bits (Bits, FiniteBits, countTrailingZeros, shift, (.&.), (.|.))
import Data.Char (ord)
import Data.Map qualified as Map
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed (MVector, Vector)
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word16, Word64, Word8)
import GHC.List (foldl')
import Text.Read (readMaybe)

newtype Bitboard = Bitboard Word64 deriving (Num, Bits, Eq, FiniteBits)
newtype instance VU.MVector s Bitboard = MV_Int (VU.MVector s Word64)
newtype instance VU.Vector Bitboard = V_Int (VU.Vector Word64)
deriving instance VGM.MVector VU.MVector Bitboard
deriving instance VG.Vector VU.Vector Bitboard
instance VU.Unbox Bitboard

type Move = Word16
data MoveType = Normal | Castle | EnPassant | Promotion deriving (Enum)
data Piece = Pawn | Rook | Knight | Bishop | Queen | King deriving (Enum, Show)
data Player = White | Black deriving (Enum, Show)
data Board = Board
  { bitbs :: Vector Bitboard
  , stm :: Player
  , castle :: ((Bool, Bool), (Bool, Bool))
  , fiftyCount :: Word16
  , ep :: Maybe Word8
  }
  deriving (Show)

genMoves :: Board -> Vector Move
genMoves Board{bitbs, stm, castle, fiftyCount, ep} = undefined

parseFen :: String -> Maybe Board
parseFen fen = splitFen (words fen) >>= parseInfo
  where
    parseInfo :: (String, String, String, String, String, String) -> Maybe Board
    parseInfo (placement, [stmRaw], castleRaw, epRaw, fiftyCount, _) = do
      bitbs <- runST $ parsePlacement placement

      stm <- case stmRaw of
        'w' -> Just White
        'b' -> Just Black
        _ -> Nothing

      ep <- case epRaw of
        "-" -> Just Nothing
        (ep1 : ep2 : _) -> do
          epCol <- parseEpCol ep1
          epRow <- readMaybe [ep2]
          Just $ Just $ fromIntegral $ posFromCoord (epCol, epRow)
        _ -> Nothing

      castle <-
        foldl'
          (\acc c -> acc >>= adjustCastle c)
          (Just ((False, False), (False, False)))
          castleRaw

      Just $ Board bitbs stm castle (read fiftyCount) ep
    parseInfo _ = Nothing

    parsePlacement :: String -> ST s (Maybe (Vector Bitboard))
    parsePlacement s = do
      result <- VGM.replicate 8 0
      finalCoord <- foldM (accumPlacement result) (Just (0, 7)) s
      case finalCoord of
        Nothing -> return Nothing
        Just _ -> do
          frozen <- VG.freeze result
          return (Just frozen)

    accumPlacement :: MVector s Bitboard -> Maybe (Int, Int) -> Char -> ST s (Maybe (Int, Int))
    accumPlacement _ Nothing _ = return Nothing
    accumPlacement result (Just (x, y)) c =
      case (c, readMaybe [c], Map.lookup c charPiece) of
        ('/', _, _) -> return $ Just (0, y - 1)
        (_, Just dx, _) -> return $ Just (x + dx, y)
        (_, _, Just (color, piece)) -> do
          VGM.modify result (.|. shift 1 (posFromCoord (x, y))) (fromEnum piece + 2)
          VGM.modify result (.|. shift 1 (posFromCoord (x, y))) (fromEnum color)
          return $ Just (x + 1, y)
        _ -> return Nothing

    charPiece =
      Map.fromList
        [ (toEnum (ord c + offset), (color, p))
        | (offset, color) <- [(0, White), (ord 'a' - ord 'A', Black)]
        , (c, p) <- [('P', Pawn), ('R', Rook), ('N', Knight), ('B', Bishop), ('Q', Queen), ('K', King)]
        ]

    adjustCastle c ((wk, wq), (bk, bq)) =
      case c of
        'K' -> Just ((True, wq), (bk, bq))
        'Q' -> Just ((wk, True), (bk, bq))
        'k' -> Just ((wk, wq), (True, bq))
        'q' -> Just ((wk, wq), (bk, True))
        _ -> Nothing

    parseEpCol c
      | c >= 'a' && c <= 'h' = Just $ ord c - ord 'a'
      | otherwise = Nothing

    splitFen [placement, stm, castle, ep, halfMove, fullMove] = Just (placement, stm, castle, ep, halfMove, fullMove)
    splitFen _ = Nothing

square1 :: Move -> Int
square1 m = fromIntegral $ m .&. 0xf

square2 :: Move -> Int
square2 m = fromIntegral $ m .&. 0xf0

moveType :: Move -> MoveType
moveType m = toEnum $ fromIntegral $ m .&. 0x300

promotion :: Move -> Piece
promotion m = toEnum $ fromIntegral $ m .&. 0x300 + 1

poplsb :: Bitboard -> Int
poplsb = countTrailingZeros

pawnMoves :: Vector Bitboard
pawnMoves = VG.generate 64 $ \i -> foldr (tryDiff 0 i) 0 [(dx, 1) | dx <- [-1, 1]]

knightMoves :: Vector Bitboard
knightMoves = VG.generate 64 $ \i -> foldr (tryDiff 0 i) 0 [(dx, dy) | dx <- [-1, -2, 1, 2], dy <- [-1, -2, 1, 2], abs dx /= abs dy]

kingMoves :: Vector Bitboard
kingMoves = VG.generate 64 $ \i -> foldr (tryDiff 0 i) 0 [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

rookXrays :: Vector Bitboard
rookXrays = VG.generate 64 $ \i -> traverseDirs 0 i [(0, 1), (1, 0), (0, -1), (-1, 0)]

bishopXrays :: Vector Bitboard
bishopXrays = VG.generate 64 $ \i -> traverseDirs 0 i [(1, 1), (1, -1), (-1, 1), (-1, -1)]

traverseDirs :: Bitboard -> Int -> [(Int, Int)] -> Bitboard
traverseDirs blockers start = foldr (\d acc -> foldr (tryDiff blockers start . (d ***)) acc [1 .. 7]) 0

tryDiff :: Bitboard -> Int -> (Int, Int) -> Bitboard -> Bitboard
tryDiff blockers start d bb =
  let p = start + posFromCoord d
   in if p >= 0 && coordFromPos p == (coordFromPos start +++ d) && blockers .&. shift 1 p == 0 then bb .|. shift 1 p else bb

posFromCoord :: (Int, Int) -> Int
posFromCoord (x, y) = x + y * 8

coordFromPos :: Int -> (Int, Int)
coordFromPos i = (col i, row i)

row :: Int -> Int
row i = div i 8

col :: Int -> Int
col i = mod i 8

infixl 7 ***
(***) :: (Int, Int) -> Int -> (Int, Int)
(***) (x, y) c = (x * c, y * c)

infixl 6 +++
(+++) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

instance Show Bitboard where
  show bb =
    unlines $
      ""
        : map
          (\y -> map (\x -> if bb .&. shift 1 (posFromCoord (x, y)) == 0 then '.' else '#') [0 .. 7])
          [7, 6 .. 0]

{-# LANGUAGE TypeFamilies #-}

module Board where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Bits (countTrailingZeros, shift, xor, (.&.), (.|.))
import Data.Char (ord)
import Data.Map qualified as Map
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed (MVector, Vector, (!))
import Data.Word (Word16, Word64, Word8)
import Text.Read (readMaybe)

type Bitboard = Word64
type Move = Word16
type Square = Word8
data MoveType = Normal | Castle | EnPassant | Promotion deriving (Enum, Show)
data Piece = Pawn | Rook | Knight | Bishop | Queen | King | Empty deriving (Enum, Show)
data Player = White | Black deriving (Enum, Show)
data Board = Board
  { bitbs :: Vector Bitboard
  , sqs :: Vector Square
  , stm :: Player
  , castle :: ((Bool, Bool), (Bool, Bool))
  , fiftyCount :: Word16
  , ep :: Maybe Word8
  }
  deriving (Show)

genMoves :: Board -> (Vector Move, Int)
genMoves Board{bitbs, stm, sqs, ep} = runST go
  where
    go :: ST s (Vector Move, Int)
    go = do
      moves <- VGM.replicate 256 0
      len <- unfoldBitboardM (addPieceMoves moves) (bitbs ! fromEnum stm, 0)
      (,len) <$> VG.freeze moves

    addPieceMoves :: MVector s Move -> Int -> Int -> ST s Int
    addPieceMoves moves sq ind = do
      let (colorRaw, piece) = squareInfo $ sqs ! sq
          color = fromEnum colorRaw
          lookupRes = lookupMoves piece sq
          canMoveTo = case piece of
            Pawn -> lookupRes .&. (bitbs ! (1 - color) .|. maybe 0 (shift 1 . fromIntegral) ep)
            _ -> lookupRes
      unfoldBitboardM
        ( \lsb l -> do
            VGM.write moves l $ newMove sq lsb Rook Normal
            return (l + 1)
        )
        (canMoveTo, ind)

parseFen :: String -> Maybe Board
parseFen fen = splitFen (words fen) >>= parseInfo
  where
    parseInfo :: (String, String, String, String, String, String) -> Maybe Board
    parseInfo (placement, [stmRaw], castleRaw, epRaw, fiftyCount, _) = do
      (bitbs, sqs) <- runST $ parsePlacement placement

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
        foldr
          (\c acc -> acc >>= adjustCastle c)
          (Just ((False, False), (False, False)))
          castleRaw

      Just $ Board bitbs sqs stm castle (read fiftyCount) ep
    parseInfo _ = Nothing

    parsePlacement :: String -> ST s (Maybe (Vector Bitboard, Vector Square))
    parsePlacement s = do
      bitbs <- VGM.replicate 8 0
      sqs <- VGM.replicate 64 $ square White Empty
      finalCoord <- foldM (accumPlacement (bitbs, sqs)) (Just (0, 7)) s
      case finalCoord of
        Nothing -> return Nothing
        Just _ -> do
          frozenBitbs <- VG.freeze bitbs
          frozenSqs <- VG.freeze sqs
          return (Just (frozenBitbs, frozenSqs))

    accumPlacement :: (MVector s Bitboard, MVector s Square) -> Maybe (Int, Int) -> Char -> ST s (Maybe (Int, Int))
    accumPlacement _ Nothing _ = return Nothing
    accumPlacement (bitbs, sqs) (Just (x, y)) c =
      case (c, readMaybe [c], Map.lookup c charPiece) of
        ('/', _, _) -> return $ Just (0, y - 1)
        (_, Just dx, _) -> return $ Just (x + dx, y)
        (_, _, Just (color, piece)) -> do
          VGM.modify bitbs (.|. shift 1 (posFromCoord (x, y))) (fromEnum piece + 2)
          VGM.modify bitbs (.|. shift 1 (posFromCoord (x, y))) (fromEnum color)
          VGM.write sqs (posFromCoord (x, y)) (square color piece)
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

square :: Player -> Piece -> Square
square c p = fromIntegral $ fromEnum c .|. shift (fromEnum p) 1

squareInfo :: Square -> (Player, Piece)
squareInfo s = (toEnum $ fromIntegral $ shift s (-1), toEnum $ fromIntegral $ s .&. 1)

newMove :: Int -> Int -> Piece -> MoveType -> Move
newMove sq1 sq2 p t = fromIntegral $ sq1 .|. shift sq2 6 .|. shift (fromEnum p - 1) 12 .|. shift (fromEnum t) 14

square1 :: Move -> Int
square1 m = fromIntegral $ m .&. 0x3f

square2 :: Move -> Int
square2 m = fromIntegral $ shift (m .&. 0xfc0) (-6)

moveType :: Move -> MoveType
moveType m = toEnum $ fromIntegral $ shift (m .&. 0x3000) (-12)

promotion :: Move -> Piece
promotion m = toEnum $ fromIntegral $ shift (m .&. 0xc000 + 1) (-14)

showMove :: Move -> String
showMove m =
  "sq1: "
    ++ show (square1 m)
    ++ ", sq2: "
    ++ show (square2 m)
    ++ ", type: "
    ++ show (moveType m)
    ++ ", prom: "
    ++ show (promotion m)

unfoldBitboardM :: (Monad m) => (Int -> a -> m a) -> (Bitboard, a) -> m a
unfoldBitboardM f (bb, start) =
  case poplsb bb of
    Just lsb -> do
      res <- f lsb start
      unfoldBitboardM f (bb `xor` shift 1 lsb, res)
    Nothing -> return start

poplsb :: Bitboard -> Maybe Int
poplsb 0 = Nothing
poplsb bb = Just $ countTrailingZeros bb

lookupMoves :: Piece -> Int -> Bitboard
lookupMoves piece sq = moveLookups ! (64 * fromEnum piece + sq)

moveLookups :: Vector Bitboard
moveLookups = foldr (VG.++) VG.empty [pawnMoves, rookXrays, knightMoves, bishopXrays, queenXrays, kingMoves]
  where
    pawnMoves = VG.generate 64 $ \i -> foldr (tryDiff 0 i) 0 [(dx, 1) | dx <- [-1, 1]]
    rookXrays = VG.generate 64 $ \i -> traverseDirs 0 i [(0, 1), (1, 0), (0, -1), (-1, 0)]
    knightMoves = VG.generate 64 $ \i -> foldr (tryDiff 0 i) 0 [(dx, dy) | dx <- [-1, -2, 1, 2], dy <- [-1, -2, 1, 2], abs dx /= abs dy]
    bishopXrays = VG.generate 64 $ \i -> traverseDirs 0 i [(1, 1), (1, -1), (-1, 1), (-1, -1)]
    queenXrays = VG.zipWith (.|.) bishopXrays rookXrays
    kingMoves = VG.generate 64 $ \i -> foldr (tryDiff 0 i) 0 [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

traverseDirs :: Bitboard -> Int -> [(Int, Int)] -> Bitboard
traverseDirs blockers start = foldr (\d acc -> foldr (tryDiff blockers start . (d ***)) acc [1 .. 7]) 0

tryDiff :: Bitboard -> Int -> (Int, Int) -> Bitboard -> Bitboard
tryDiff blockers start d bb =
  let p = start + posFromCoord d
   in if p >= 0 && coordFromPos p == (coordFromPos start +++ d) && blockers .&. shift 1 p == 0 then bb .|. shift 1 p else bb

showBitboard :: Bitboard -> String
showBitboard bb =
  unlines $
    ""
      : map
        (\y -> map (\x -> if bb .&. shift 1 (posFromCoord (x, y)) == 0 then '.' else '#') [0 .. 7])
        [7, 6 .. 0]

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
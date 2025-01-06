{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Board where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Bits (bit, complement, countTrailingZeros, popCount, shiftL, shiftR, testBit, xor, (.&.), (.^.), (.|.))
import Data.Char (ord)
import Data.List (unfoldr)
import Data.Map qualified as Map
import Data.Massiv.Array (Array, (!))
import Data.Massiv.Array qualified as A
import Data.Massiv.Array.Mutable (MArray)
import Data.Massiv.Array.Mutable qualified as MA
import Data.Word (Word16, Word64, Word8)
import GHC.Exts (Word (W#), inline, pdep#)
import System.Random (mkStdGen, uniform)
import Text.Read (readMaybe)

type Bitboard = Word64
type Move = Word16
type Square = Word8
data MoveType = Normal | Castle | EnPassant | Promotion deriving (Enum, Show)
data Piece = Rook | Bishop | Pawn | Knight | King | Queen | Empty deriving (Enum, Show)
data Player = White | Black deriving (Enum, Show)
data Board = Board
  { bitbs :: Array A.P Int Bitboard
  , sqs :: Array A.P Int Square
  , stm :: Player
  , castle :: ((Bool, Bool), (Bool, Bool))
  , fiftyCount :: Word16
  , ep :: Maybe Word8
  }
  deriving (Show)

genMoves :: Board -> (Array A.P Int Move, Int)
genMoves Board{bitbs, stm, sqs, ep} = runST $
  do
    moves <- MA.thawS $ A.replicate A.Seq 256 0
    len <- foldBitboardM (addPieceMoves moves) (bitbs ! fromEnum stm) 0
    (,len) <$> MA.freezeS moves
  where
    addPieceMoves :: MArray s A.P Int Move -> Int -> Int -> ST s Int
    addPieceMoves moves sq ind = do
      let (piece, _) = squareInfo $ sqs ! sq
          canMoveTo =
            complement (bitbs ! fromEnum stm)
              .&. case piece of
                Pawn -> (moveLookups ! (fromEnum piece, sq)) .&. (bitbs ! (1 - fromEnum stm) .|. maybe 0 (bit . fromIntegral) ep)
                Rook -> getSlidingAttack 0 sq
                Bishop -> getSlidingAttack 1 sq
                Queen -> getSlidingAttack 0 sq .|. getSlidingAttack 1 sq
                _ -> moveLookups ! (fromEnum piece, sq)
      foldBitboardM
        ( \lsb l -> do
            MA.write_ moves l $ newMove sq lsb Rook Normal
            return (l + 1)
        )
        canMoveTo
        ind

    getSlidingAttack p sq =
      let blockers = inline $ (moveLookups ! (p, sq)) .&. ((bitbs ! fromEnum White) .|. (bitbs ! fromEnum Black))
          ind = inline $ shiftR (blockers * fromIntegral (magicNums ! (p, sq))) (magicShift ! (p, sq))
       in magicLookup ! (p, sq, fromIntegral ind)

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

    parsePlacement :: String -> ST s (Maybe (Array A.P Int Bitboard, Array A.P Int Square))
    parsePlacement s = do
      bitbs <- MA.thawS $ A.replicate A.Seq 8 0
      sqs <- MA.thawS $ A.replicate A.Seq 64 $ square White Empty
      finalCoord <- foldM (accumPlacement (bitbs, sqs)) (Just (0, 7)) s
      case finalCoord of
        Nothing -> return Nothing
        Just _ -> do
          frozenBitbs <- MA.freezeS bitbs
          frozenSqs <- MA.freezeS sqs
          return (Just (frozenBitbs, frozenSqs))

    accumPlacement :: (MArray s A.P Int Bitboard, MArray s A.P Int Square) -> Maybe (Int, Int) -> Char -> ST s (Maybe (Int, Int))
    accumPlacement _ Nothing _ = return Nothing
    accumPlacement (bitbs, sqs) (Just (x, y)) c =
      case (c, readMaybe [c], Map.lookup c charPiece) of
        ('/', _, _) -> return $ Just (0, y - 1)
        (_, Just dx, _) -> return $ Just (x + dx, y)
        (_, _, Just (color, piece)) -> do
          MA.modify_ bitbs (return . xor (bit $ posFromCoord (x, y))) (fromEnum piece + 2)
          MA.modify_ bitbs (return . xor (bit $ posFromCoord (x, y))) (fromEnum color)
          MA.write_ sqs (posFromCoord (x, y)) (square color piece)
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

{-# INLINE square #-}
square :: Player -> Piece -> Square
square c p = fromIntegral $ shiftL (fromEnum p) 1 .^. fromEnum c

{-# INLINE squareInfo #-}
squareInfo :: Square -> (Piece, Player)
squareInfo s = (toEnum $ fromIntegral $ shiftR s 1, toEnum $ fromIntegral $ s .&. 1)

{-# INLINE newMove #-}
newMove :: Int -> Int -> Piece -> MoveType -> Move
newMove sq1 sq2 p t = fromIntegral $ sq1 .^. shiftL sq2 6 .^. shiftL (fromEnum p) 12 .^. shiftL (fromEnum t) 14

{-# INLINE square1 #-}
square1 :: Move -> Int
square1 m = fromIntegral $ m .&. 0x3f

{-# INLINE square2 #-}
square2 :: Move -> Int
square2 m = fromIntegral $ shiftR (m .&. 0xfc0) 6

{-# INLINE moveType #-}
moveType :: Move -> MoveType
moveType m = toEnum $ fromIntegral $ shiftR (m .&. 0x3000) 12

{-# INLINE promotion #-}
promotion :: Move -> Piece
promotion m = toEnum $ fromIntegral $ shiftR (m .&. 0xc000 + 1) 14

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

foldBitboardM :: (Monad m) => (Int -> a -> m a) -> Bitboard -> a -> m a
foldBitboardM f bb start =
  case poplsb bb of
    Just lsb -> do
      res <- f lsb start
      foldBitboardM f (bb .^. bit lsb) res
    Nothing -> return start

magicLookup :: Array A.P (Int, Int, Int) Bitboard
magicLookup = runST $
  do
    result <- MA.thawS $! A.replicate A.Seq (A.Sz (2, 64, 4096)) 0
    mapM_ (squareMagicLookups result) [(p, sq) | p <- [0, 1], sq <- [0 .. 63]]
    MA.freezeS result
  where
    squareMagicLookups :: MArray s A.P (Int, Int, Int) Bitboard -> (Int, Int) -> ST s ()
    squareMagicLookups arr (p, sq) =
      let xray = moveLookups ! (p, sq)
          num1s = popCount xray
       in mapM_
            ( \bb ->
                MA.write_
                  arr
                  (p, sq, fromIntegral $ shiftR (bb * (magicNums ! (p, sq))) (64 - num1s))
                  (traverseDirs bb sq (slideDirs ! p))
            )
            [intToBitboard xray i | i <- init [0 .. 2 ^ num1s]]

magicNums :: Array A.P (Int, Int) Word64
magicNums = A.makeArray A.Seq (A.Sz (2, 64)) (uncurry getMagic)

getMagic :: Int -> Int -> Word64
getMagic p sq =
  let xray = moveLookups ! (p, sq)
   in head
        $ filter
          (checkMagic (magicShift ! (p, sq)) xray (popCount xray))
        $ makeSparse
        $ unfoldr (Just . uniform) (mkStdGen $ fromIntegral xray)

makeSparse :: [Word64] -> [Word64]
makeSparse (l1 : l2 : l3 : ltail) = l1 .&. l2 .&. l3 : makeSparse ltail
makeSparse l = l

{-# INLINE checkMagic #-}
checkMagic :: Int -> Bitboard -> Int -> Word64 -> Bool
checkMagic shift xray num1s magic = runST $ do
  vis :: MArray s A.U Int Bool <- MA.newMArray (2 ^ num1s) False
  let
    go 0 = return True
    go i = do
      let ind = fromIntegral $ shiftR (magic * intToBitboard xray i) shift
      isRepeat <- MA.readM vis ind
      if isRepeat
        then return False
        else do
          MA.write_ vis ind True
          go (i - 1)
  go (2 ^ num1s)

magicShift :: Array A.P (Int, Int) Int
magicShift = A.makeArray A.Seq (A.Sz (2, 64)) (\ix -> 64 - popCount (moveLookups ! ix))

{-# INLINE intToBitboard #-}
intToBitboard :: Bitboard -> Word64 -> Bitboard
intToBitboard bb i = fromIntegral $ pdepRaw (fromIntegral bb) (fromIntegral i)
  where
    pdepRaw (W# bb#) (W# x#) = W# (pdep# x# bb#)

moveLookups :: Array A.P (Int, Int) Bitboard
moveLookups = A.convert $ runST (A.stackOuterSlicesM [rookXrays, bishopXrays, pawnMoves, knightMoves, kingMoves])
  where
    pawnMoves, rookXrays, knightMoves, bishopXrays, kingMoves :: Array A.P Int Bitboard
    pawnMoves = A.makeArray A.Seq 64 $ \i -> foldr (tryDiff i) 0 [(dx, 1) | dx <- [-1, 1]]
    knightMoves = A.makeArray A.Seq 64 $ \i -> foldr (tryDiff i) 0 [(dx, dy) | dx <- [-1, -2, 1, 2], dy <- [-1, -2, 1, 2], abs dx /= abs dy]
    kingMoves = A.makeArray A.Seq 64 $ \i -> foldr (tryDiff i) 0 [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

    rookXrays = A.makeArray A.Seq 64 $ genXray 0
    bishopXrays = A.makeArray A.Seq 64 $ genXray 1
    genXray p sq =
      let blockers = foldr (\side acc -> if not $ testBit side sq then acc .|. side else acc) 0 sides
       in complement blockers .&. traverseDirs blockers sq (slideDirs ! p)
    sides = [0xff, 0xff00000000000000, 0x0101010101010101, 0x8080808080808080]

slideDirs :: Array A.B Int [(Int, Int)]
slideDirs = A.fromList A.Seq [[(0, 1), (1, 0), (0, -1), (-1, 0)], [(1, 1), (1, -1), (-1, 1), (-1, -1)]]

traverseDirs :: Bitboard -> Int -> [(Int, Int)] -> Bitboard
traverseDirs blockers start =
  foldr
    ( \d acc ->
        foldr
          (tryDiff start . (d ***))
          acc
          (takeWhile (\i -> notHitsBlocker d (i - 1)) [1 .. 7])
    )
    0
  where
    notHitsBlocker d i =
      let p = posFromCoord $ coordFromPos start +++ d *** i
       in p >= 0 && not (testBit blockers p)

tryDiff :: Int -> (Int, Int) -> Bitboard -> Bitboard
tryDiff start d bb =
  let p = start + posFromCoord d
   in if p >= 0 && coordFromPos p == (coordFromPos start +++ d) then bb .^. bit p else bb

{-# INLINE poplsb #-}
poplsb :: Bitboard -> Maybe Int
poplsb 0 = Nothing
poplsb bb = Just $ countTrailingZeros bb

showBitboard :: Bitboard -> String
showBitboard bb =
  unlines $
    ""
      : map
        (\y -> map (\x -> if not $ testBit bb $ posFromCoord (x, y) then '.' else '#') [0 .. 7])
        [7, 6 .. 0]

{-# INLINE posFromCoord #-}
posFromCoord :: (Int, Int) -> Int
posFromCoord (x, y) = x + y * 8

{-# INLINE coordFromPos #-}
coordFromPos :: Int -> (Int, Int)
coordFromPos i = (col i, row i)

{-# INLINE row #-}
row :: Int -> Int
row i = div i 8

{-# INLINE col #-}
col :: Int -> Int
col i = mod i 8

{-# INLINE (***) #-}
infixl 7 ***
(***) :: (Int, Int) -> Int -> (Int, Int)
(***) (x, y) c = (x * c, y * c)

{-# INLINE (+++) #-}
infixl 6 +++
(+++) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

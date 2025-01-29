{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Board where

import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (bit, complement, countTrailingZeros, popCount, shiftL, shiftR, testBit, xor, (.&.), (.^.), (.|.))
import Data.Char (chr, ord)
import Data.List (unfoldr)
import Data.List.Split (chunksOf)
import Data.Map qualified as Map
import Data.Massiv.Array (Array, (!))
import Data.Massiv.Array qualified as A
import Data.Massiv.Array.Mutable (MArray)
import Data.Massiv.Array.Mutable qualified as MA
import Data.Maybe (fromJust)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word16, Word64, Word8)
import Debug.Trace
import GHC.Exts (Word (W#), inline, pdep#)
import Numeric (showBin)
import System.Random (mkStdGen, uniform)
import Text.Read (readMaybe)

type Bitboard = Word64
type Move = Word16
type Square = Word8
data MoveType = Normal | Castle | EnPassant | Promotion deriving (Enum, Show, Eq)
data Piece = Rook | Bishop | Pawn | Knight | King | Queen | Empty deriving (Enum, Show, Eq, Bounded)
data Player = White | Black deriving (Enum, Show)
data Board s = Board
  { bitbs :: MArray s A.P Int Bitboard
  , sqs :: MArray s A.P Int Square
  , castle :: MArray s A.P Int Int
  , history :: MArray s A.P Int Move
  , captured :: MArray s A.P Int Square
  , fiftyCounts :: MArray s A.P Int Int
  , other :: STRef s (Player, Maybe Int, Int)
  }

genMoves :: Board s -> ST s (Array A.P Int Move, Int)
genMoves Board{bitbs, sqs, other} = do
  (stm, _, _) <- readSTRef other
  moves <- MA.thawS $ A.replicate A.Seq 256 0
  myPieces <- MA.readM bitbs $ fromEnum stm
  len <- foldBitboardM (addPieceMoves moves) myPieces 0
  moves' <- MA.freezeS moves
  return (moves', len)
  where
    addPieceMoves moves sq ind = do
      (stm, ep, _) <- readSTRef other
      (piece, _) <- squareInfo <$> MA.readM sqs sq
      myPieces <- MA.readM bitbs $ fromEnum stm
      oppPieces <- MA.readM bitbs $ fromEnum $ flipColor stm
      canMoveTo <-
        (complement myPieces .&.)
          <$> case piece of
            Pawn -> return $ (moveLookups ! (fromEnum piece, sq)) .&. (oppPieces .|. maybe 0 (bit . fromIntegral) ep)
            Rook -> getSlidingAttack 0 sq
            Bishop -> getSlidingAttack 1 sq
            Queen -> do
              rookAttacks <- getSlidingAttack 0 sq
              bishopAttacks <- getSlidingAttack 1 sq
              return $ rookAttacks .|. bishopAttacks
            _ -> return $ moveLookups ! (fromEnum piece, sq)
      foldBitboardM
        ( \lsb l -> do
            MA.write_ moves l $ newMove sq lsb Rook Normal
            return (l + 1)
        )
        canMoveTo
        ind

    getSlidingAttack p sq = do
      whitePieces <- inline $ MA.readM bitbs $ fromEnum White
      blackPieces <- inline $ MA.readM bitbs $ fromEnum Black
      let blockers = inline $ (moveLookups ! (p, sq)) .&. (whitePieces .|. blackPieces)
          ind = inline $ shiftR (blockers * fromIntegral (magicNums ! (p, sq))) (magicShift ! (p, sq))
      return $ magicLookup ! (p, sq, fromIntegral ind)

makeMove :: Board s -> Move -> ST s Bool
makeMove Board{bitbs, fiftyCounts, sqs, captured, castle, other, history} m = do
  (stm, ep, gameLen) <- readSTRef other
  let (sq1, sq2, moveT, prom) = moveInfo m
  (toPiece, _) <- squareInfo <$> MA.readM sqs sq2
  (fromPiece, fromColor) <- squareInfo <$> MA.readM sqs sq1
  fiftyCount <- MA.readM fiftyCounts gameLen

  traceShow fromPiece $ MA.modifyM_ bitbs (return . xor (bit sq1)) $ pieceInd fromPiece
  MA.writeM sqs sq1 $ square White Empty

  let endPiece = traceShowId $ if moveT == Promotion then prom else fromPiece
  MA.modifyM_ bitbs (return . xor (bit sq2)) $ pieceInd endPiece
  MA.modifyM_ bitbs (return . xor (bit sq1 .|. bit sq2)) $ fromEnum stm
  MA.writeM sqs sq2 $ square fromColor endPiece

  if traceShowId moveT == EnPassant
    then do
      let epSq = fromJust ep
          captureSq = inline $ fromIntegral epSq - colorDir stm * 8
      MA.writeM sqs captureSq $ square White Empty
      MA.modifyM_ bitbs (return . xor (bit captureSq)) $ pieceInd Pawn
    else when (toPiece /= Empty) $ MA.modifyM_ bitbs (return . xor (bit sq2)) $ pieceInd toPiece

  let newEp = fromBool (fromPiece == Pawn && abs (sq1 - sq2) == 16) $ div (sq1 + sq2) 2
      modifyRookCastle inStm sq =
        when
          (row sq `mod` 7 == 0 && col sq `mod` 7 == 0)
          $ MA.modifyM_ castle (return . min gameLen) (fromEnum inStm * 2 + col sq `div` 2)

  when (trace (showBin m "") moveT == Castle) $ do
    let endRookSq = inline $ (sq1 + sq2) `div` 2
        startRookSq = inline $ 56 * fromEnum (row sq1 == 7) + 7 * fromEnum (sq1 < sq2)
        moveRookBB = return . xor (bit endRookSq .|. bit startRookSq)
    MA.modifyM_ bitbs moveRookBB $ pieceInd Rook
    MA.modifyM_ bitbs moveRookBB $ fromEnum stm
    MA.write_ sqs startRookSq $ square White Empty
    MA.write_ sqs endRookSq $ square stm Rook

  when (fromPiece == King) $ do
    MA.modifyM_ castle (return . min gameLen) $ fromEnum stm * 2
    MA.modifyM_ castle (return . min gameLen) $ fromEnum stm * 2 + 1
  modifyRookCastle stm sq1
  modifyRookCastle (flipColor stm) sq2

  MA.write_ captured gameLen $ square (flipColor stm) toPiece
  MA.write_ history gameLen m
  MA.write_ fiftyCounts (gameLen + 1) $
    if fromPiece == Pawn || toPiece /= Empty || moveT == Castle
      then fiftyCount + 1
      else 0

  writeSTRef other (flipColor stm, newEp, gameLen + 1)

  s <- showMArr showBitboard bitbs
  s2 <- showSqs sqs
  return $ trace (s ++ s2) True

unmakeMove :: Board s -> ST s ()
unmakeMove Board{bitbs, sqs, captured, castle, other, history} = do
  (stmPrev, ep, gameLen) <- readSTRef other
  let stm = flipColor stmPrev
  (sq1, sq2, moveT, prom) <- moveInfo <$> MA.readM history gameLen
  (endPiece, endCol) <- squareInfo <$> MA.readM sqs sq2
  capturedSquare <- MA.readM captured gameLen
  let startPiece = if moveT == Promotion then prom else endPiece

  MA.modifyM_ bitbs (return . xor (bit sq1)) $ pieceInd startPiece
  MA.modifyM_ bitbs (return . xor (bit sq2)) $ pieceInd endPiece
  MA.writeM sqs sq2 capturedSquare
  MA.writeM sqs sq1 $ square stm startPiece

  when (moveT == Castle) $ do
    let endRookSq = inline $ (sq1 + sq2) `div` 2
        startRookSq = inline $ 56 * fromEnum (row sq1 == 7) + 7 * fromEnum (sq1 < sq2)
        moveRookBB = return . xor (bit endRookSq .|. bit startRookSq)
    MA.modifyM_ bitbs moveRookBB $ pieceInd Rook
    MA.modifyM_ bitbs moveRookBB $ fromEnum stm
    MA.write_ sqs startRookSq $ square stm Rook
    MA.write_ sqs endRookSq $ square White Empty

  MA.forPrimM castle (\turn -> if turn == gameLen then return maxBound else return turn)

  writeSTRef other (stm, Nothing, gameLen - 1)

  s <- showMArr showBitboard bitbs
  s2 <- showSqs sqs
  return $ trace (s ++ s2) ()

parseFen :: String -> ST s (Maybe (Board s))
parseFen fen = maybe (return Nothing) parseInfo $ splitFen $ words fen
  where
    parseInfo :: (String, String, String, String, String, String) -> ST s (Maybe (Board s))
    parseInfo (placementStr, [stmRaw], castleRaw, epRaw, fiftyCountRaw, _) =
      do
        history <- MA.newMArray 4096 0
        captured <- MA.newMArray 4096 $ square White Empty
        placement <- parsePlacement placementStr
        fiftyCounts <- MA.newMArray 4096 0
        MA.writeM fiftyCounts (read fiftyCountRaw) 0
        castle <- MA.thawS $ A.fromList A.Seq [-1, -1, -1, -1]
        forM_ castleRaw (\c -> MA.writeM castle (castleInd c) maxBound)

        let res = do
              (bitbs, sqs) <- placement

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

              Just (bitbs, sqs, stm, ep)
        maybe
          (return Nothing)
          (\(bitbs, sqs, stm, ep) -> Just . Board bitbs sqs castle history captured fiftyCounts <$> newSTRef (stm, ep, 0))
          res
    parseInfo _ = return Nothing

    parsePlacement :: String -> ST s (Maybe (MArray s A.P Int Bitboard, MArray s A.P Int Square))
    parsePlacement s = do
      bitbs <- MA.newMArray 8 0
      sqs <- MA.newMArray 64 (square White Empty)
      finalCoord <- foldM (accumPlacement (bitbs, sqs)) (Just (0, 7)) s
      case finalCoord of
        Nothing -> return Nothing
        Just _ -> do
          return (Just (bitbs, sqs))

    accumPlacement :: (MArray s A.P Int Bitboard, MArray s A.P Int Square) -> Maybe (Int, Int) -> Char -> ST s (Maybe (Int, Int))
    accumPlacement _ Nothing _ = return Nothing
    accumPlacement (bitbs, sqs) (Just (x, y)) c =
      case (c, readMaybe [c], Map.lookup c charPiece) of
        ('/', _, _) -> return $ Just (0, y - 1)
        (_, Just dx, _) -> return $ Just (x + dx, y)
        (_, _, Just (color, piece)) -> do
          MA.modify_ bitbs (return . xor (bit $ posFromCoord (x, y))) $ pieceInd piece
          MA.modify_ bitbs (return . xor (bit $ posFromCoord (x, y))) $ fromEnum color
          MA.write_ sqs (posFromCoord (x, y)) (square color piece)
          return $ Just (x + 1, y)
        _ -> return Nothing

    castleInd c = snd $ head $ filter ((== c) . fst) [('K', 0), ('Q', 1), ('k', 2), ('q', 3)]

    charPiece =
      Map.fromList
        [ (toEnum (ord c + offset), (color, p))
        | (offset, color) <- [(0, White), (ord 'a' - ord 'A', Black)]
        , (c, p) <- [('P', Pawn), ('R', Rook), ('N', Knight), ('B', Bishop), ('Q', Queen), ('K', King)]
        ]

    parseEpCol c
      | c >= 'a' && c <= 'h' = Just $ ord c - ord 'a'
      | otherwise = Nothing

    splitFen [placement, stm, castle, ep, halfMove, fullMove] = Just (placement, stm, castle, ep, halfMove, fullMove)
    splitFen _ = Nothing

{-# INLINE pieceInd #-}
pieceInd :: Piece -> Int
pieceInd = (+ 2) . fromEnum

{-# INLINE fromBool #-}
fromBool :: Bool -> a -> Maybe a
fromBool b = if b then Just else const Nothing

{-# INLINE colorDir #-}
colorDir :: Player -> Int
colorDir White = 1
colorDir Black = -1

{-# INLINE flipColor #-}
flipColor :: Player -> Player
flipColor p = toEnum $ 1 - fromEnum p

{-# INLINE square #-}
square :: Player -> Piece -> Square
square c p = fromIntegral $ shiftL (fromEnum p) 1 .^. fromEnum c

{-# INLINE squareInfo #-}
squareInfo :: Square -> (Piece, Player)
squareInfo s = (toEnum $ fromIntegral $ shiftR s 1, toEnum $ fromIntegral $ s .&. 1)

{-# INLINE newMove #-}
newMove :: Int -> Int -> Piece -> MoveType -> Move
newMove sq1 sq2 p t = fromIntegral $ sq1 .^. shiftL sq2 6 .^. shiftL (fromEnum p) 12 .^. shiftL (fromEnum t) 14

{-# INLINE moveInfo #-}
moveInfo :: Move -> (Int, Int, MoveType, Piece)
moveInfo m = (square1 m, square2 m, moveType m, promotion m)

{-# INLINE square1 #-}
square1 :: Move -> Int
square1 m = fromIntegral $ m .&. 0x3f

{-# INLINE square2 #-}
square2 :: Move -> Int
square2 m = fromIntegral $ shiftR (m .&. 0xfc0) 6

{-# INLINE promotion #-}
promotion :: Move -> Piece
promotion m = toEnum $ fromIntegral $ shiftR (m .&. 0x3000 + 1) 12

{-# INLINE moveType #-}
moveType :: Move -> MoveType
moveType m = toEnum $ fromIntegral $ shiftR (m .&. 0xc000) 14

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

showMArr :: (A.Prim a) => (a -> String) -> MArray s A.P Int a -> ST s String
showMArr f arr = do
  l <- A.toList <$> MA.freezeS arr
  return $ unlines $ map f l

showSqs :: MArray s A.P Int Square -> ST s String
showSqs arr = do
  l <- A.toList <$> MA.freezeS arr
  return $ unlines $ chunksOf 8 $ map (squareChar . squareInfo) l
  where
    squareChar (p, c) =
      chr $
        fromIntegral (fromEnum c) * (ord 'a' - ord 'A') + case p of
          Rook -> ord 'R'
          Bishop -> ord 'B'
          Pawn -> ord 'P'
          Knight -> ord 'N'
          King -> ord 'K'
          Queen -> ord 'Q'
          _ -> ord '.'

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

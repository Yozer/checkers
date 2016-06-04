

module Board where

import           Data.Bits
import           Data.List
import           Data.List.Split
import           Data.Word
import           Masks
import qualified Data.Vector.Unboxed as V
import Debug.Trace



data Figure = BlackPiece | WhitePiece | BlackKing | WhiteKing | Empty | Special deriving (Show, Eq)
data Player = White | Black deriving(Show, Eq)

data Board = Board {
  wp :: {-# UNPACK #-} !Word64,
  bp :: {-# UNPACK #-} !Word64,
  k  :: {-# UNPACK #-} !Word64
} deriving (Show, Eq)

data GameState = GameState Board Player Word64 deriving (Eq, Show)

type Path = [Word64]
type MovesList = [Path]

data MoveHolder =  None | JumpMove Path | NormalMove Path deriving(Show, Eq, Ord)

------ HELPERS

empty :: Board -> Word64
empty b = (complement (bp b .|. wp b)) .&. bitsOnTheBoard


whiteKings :: Board -> Word64
whiteKings b = k b .&. wp b


blackKings :: Board -> Word64
blackKings b = k b .&. bp b


blackPieces :: Board -> Word64
blackPieces b = (complement $ k b) .&. bp b


whitePieces :: Board -> Word64
whitePieces b = (complement $ k b) .&. wp b


isKing :: Board -> Word64 -> Bool
isKing b i = k b .&. i /= 0


isWhiteKing :: Board -> Word64 -> Bool
isWhiteKing b i = k b .&. i /= 0 && wp b .&. i /= 0


isBlackKing :: Board -> Word64 -> Bool
isBlackKing b i = k b .&. i /= 0 && bp b .&. i /= 0


upLeft :: Word64 -> Word64
upLeft x = unsafeShiftL x 1


upRight :: Word64 -> Word64
upRight x = unsafeShiftL x 9


downRight :: Word64 -> Word64
downRight x = unsafeShiftR x 1


downLeft :: Word64 -> Word64
downLeft x = unsafeShiftR x 9


removePieceByIndex :: Word64 -> Word64 -> Word64
removePieceByIndex x i = x `xor` i


whiteBottomEdge :: Word64
whiteBottomEdge = 16843009


blackBottomEdge :: Word64
blackBottomEdge = 9259542121117908992



hashTable :: Int -> Word64
hashTable i = V.fromList ([7964275156172250285, 10251665355298115738, 14020150906999509392, 5918195500845856164, 5390692505172812698, 17865876418749382761, 11059545099175768107, 8321467454596635402, 8847475449804613359, 14794199428220864705, 13053451384527045765, 2178394641088189991, 2191915737698050629, 18237964409428496103, 4833689763218515851, 835520947355673023, 14234827805974251021, 7148836553251090776, 964736934773403900, 702410596842951053, 14340794174806468405, 8919946256620638043, 2119257533888111916, 7101664206236116067, 13804090788383772978, 770145211587204907, 10955587172894396550, 12488163930223851326, 12793453481026818113, 14528638781470456083, 11629557553773364403, 14794785628991177286, 10115688409655800136, 11968636580811112901, 4914455505943498006, 17225655495579465003, 1028284186703639911, 11786941112665674722, 2382309003161103273, 8261006759536184510, 7070946648227876596, 740625280078883656, 4469108155858231394, 7254234615400824102, 2062430122589614063, 350265796063051722, 18285220600436471301, 10443962703376785033, 12125474935455539710, 4745744147350167424, 5234287278992640544, 16275473619698943533, 3264556301845348046, 14075244670137369455, 10557479545557316034, 16654770960693981758, 10959751057124774209, 6724861244493006204, 11145258664676500017, 5673303397646839738, 7403472129307498593, 4424077344461245529, 15542640010883615134, 6752971830332524453, 5129371967902186826, 13777632153850084451, 6129657622352626761, 8704588615746685222, 17158031594202962580, 3018900835924752026, 16049457191036186708, 17981758072672867053, 3993648981297283887, 6666540752811370982, 17631511573670515876, 1277903974900172511, 9877254307950228009, 2152808735807886724, 17878238527988355149, 6904417688902655891, 1541770107708074989, 1103400082990286784, 7375170592379658022, 11005535344827898069, 6259532954296536835, 284172617485507691, 10253497582905485326, 13510603163231934994, 5944919034276654091, 13864588466784361600, 14222310571793577255, 11445960707232080643, 17737066564817314397, 12199851262223488744, 13699899781126452466, 14354041780238106189, 17163103301854202371, 17163078947903411151, 781884408796904532, 11277977205277402562, 15754840793205516131, 5467538231697936197, 13009783364107224591, 16423322803918065584, 7220577144968226831, 12140905897566431191, 15641297303439063108, 7441710563636979098, 16871233484337763619, 548842277089214609, 9393434167571379025, 1409567077871503766, 1995403085526596896, 4049983385872586901, 8452107688629261063, 5233216906337295737, 13402674395357514736, 12147569400123588418, 12999974234338682004, 17694363986038264241, 12204207344859833016, 2518366673986051924, 4690262767903089929, 13829732668565442209, 449287121237790704, 2886454490123391621, 1104059107572229022, 13804261390069912394, 6368463156607049458, 7039485789316748137, 672804586576858431, 8065558184096758525, 14639012188025446975, 9477545883579713602, 2957632865604395042, 5362084006342439047, 9789558300278371237, 8990311903644409765, 2342754991351812466, 14937775175371470039, 2493696701419778462, 476783355512575748, 110860075233631083, 9414217380504891440, 10307810810210902161, 8624664856819837365, 4947662208200549270, 9281187753412865767, 3977808019717759920, 13367704388684415951, 8174352842142537151, 14913825000207337910, 1415575023459600859, 14372020680096330735, 965491926788712362, 14043368579933931697, 2179084057888008202, 9964465336214423983, 4504505406621051733, 6791741916484053795] :: [Word64])
            V.! i
hashFunction :: Word64 -> Board -> Word64
hashFunction index board = hashTable tableIndex
  where
    pieceType
      | wp board .&. index /= 0 = if k board .&. index /= 0 then 1 else 0
      | k board .&. index /= 0 = 2
      | otherwise = 3
    square = rfield index
    tableIndex = square*4 + pieceType + 1

flipPlayerHash :: Word64
flipPlayerHash = hashTable 0

hashBoard :: Board -> Player -> Word64
hashBoard board player = result'
  where
      result = foldl xor  (0 :: Word64) . map (\(a,_) -> hashFunction a board) . filter (\(_,a) -> a /= Empty) $ zip (getBoardFields [1..32]) (map (getPiece board) $ getBoardFields [1..32])
      result' = if player == Black then result `xor` flipPlayerHash else result

hashGameState :: GameState -> Word64
hashGameState (GameState board player _) = hashBoard board player


-- END OF HELPERS

--- MOVE AND JUMPS EVALUATION

getNextPlayer :: Player -> Player
getNextPlayer x = if x == White then Black else White


movePiece :: Word64 -> Word64 -> Word64 -> Word64 -- make a normal move, no validation performed
movePiece x from to = (removePieceByIndex x from) .|. to


promotePiece :: Word64 -> Player -> Word64 -> Word64 -- promote piece if needed
promotePiece dst player kings
  | (player == White && dst .&. blackBottomEdge /= 0) || (player == Black && dst .&. whiteBottomEdge /= 0) = kings .|. dst
  | otherwise = kings


doMove :: GameState -> MoveHolder -> GameState
doMove (GameState board player hash) (NormalMove (from:to:[])) = GameState board' (getNextPlayer player) hash'
  where
    whitePieces' = movePiece (wp board) from to -- move piece for white
    blackPieces' = movePiece (bp board) from to -- move piece for black
    kings = if isKing board from then movePiece (k board) from to else promotePiece to player (k board)
    board'
        | player == White = board {wp = whitePieces', k = kings}
        | otherwise = board {bp = blackPieces', k = kings}
    hash' = ((hash `xor` flipPlayerHash) `xor` (hashFunction from board)) `xor` (hashFunction to board')


doMove (GameState board player hash) (JumpMove jump) = GameState result (getNextPlayer player) hash''
  where
    opponent = if player == White then bp board else wp board
    (killedPieces, hash') = doJump' board opponent hash jump 
    from = head jump
    to = last jump

    negatedKilledPieces = complement killedPieces

    whitePieces' = if player == White then movePiece (wp board) from to else  negatedKilledPieces .&. wp board
    blackPieces' = if player == Black then movePiece (bp board) from to else negatedKilledPieces .&. bp board
    kings = negatedKilledPieces .&. k board
    kings' = if isKing board from then movePiece kings from to else promotePiece to player kings
    result = Board {wp = whitePieces', bp = blackPieces', k=kings'}
    hash'' = ((hash' `xor` flipPlayerHash) `xor` (hashFunction from board)) `xor` (hashFunction to result)
    
doMove g _ = g

doJump' :: Board -> Word64 -> Word64 -> Path -> (Word64, Word64)
doJump' board opponent hash (a:path)
  | path == [] = (0, hash)
  | otherwise = (killedPiece .|. recur', hash' `xor` (hashFunction killedPiece board))
  where
    b = head path
    direction = getMoveDirection a b

    --opponent = if player == White then bp board else wp board
    killedPiece = getKilledPiece opponent direction a
    (recur', hash') = doJump' board opponent hash path

--- END OF MOVE AND JUMPS EVALUATION


-- check if game has ended

isGameEnded :: Board -> Bool
isGameEnded board = bp board == 0 || wp board == 0


------ HELPERS -----

getKilledPiece :: Word64 -> (Word64 -> Word64) -> Word64 -> Word64
getKilledPiece opponent direction position
  | killed /= 0 = killed
  | otherwise = getKilledPiece opponent direction (direction position)
  where

    killed = position .&. opponent


getMoveDirection :: Word64 -> Word64 -> (Word64 -> Word64)
getMoveDirection source destination
  | res >= (9 :: Int) = upRight
  | res > 0 = upLeft
  | res <= -9 = downLeft
  | otherwise = downRight
  where

    res = (fromIntegral $ getIndex destination) - (fromIntegral $ getIndex source)

---------------------- display -------------------------

initialBoard :: Board
initialBoard =
  let
    white = mergeBoardFields [1..12]
    black = mergeBoardFields [21..32]
    kings = 0
  in Board {wp = white, bp = black, k = kings}

initialGameState :: GameState
initialGameState = GameState initialBoard White (hashBoard initialBoard White)

printBoard :: Board -> String -- prints board
printBoard board = unlines . (++["  1 2 3 4 5 6 7 8"])  . zipWith (++) (map (:" ") ['8','7'..'1']) . chunksOf 16 . intersperse ' ' . map (getFigureChar . getPiece board . mapBoard) $ [0..63]


printBoardWithPath :: Board -> Path -> String -- prints board
printBoardWithPath board m = unlines . (++["  1 2 3 4 5 6 7 8"])  . zipWith (++) (map (:" ") ['8','7'..'1'])
                              . chunksOf 16 . intersperse ' ' . map (\x -> getFigureChar $ if (mapBoard $ fromIntegral x) `elem` m then Special else (getPiece board (mapBoard $ fromIntegral x))) $ ([0..63] :: [Int])



getPiece :: Board -> Word64 -> Figure -- get piece [0..64]
getPiece b val
  | val == invalid = Empty
  | isBlack && isKing b val = BlackKing
  | isBlack = BlackPiece
  | isWhite && isKing b val = WhiteKing
  | isWhite = WhitePiece
  | otherwise = Empty
  where
    isBlack = bp b .&. val /= 0
    isWhite = wp b .&. val /= 0

getFigureChar :: Figure -> Char
getFigureChar x
  | x == WhitePiece = 'w'
  | x == BlackPiece = 'b'
  | x == BlackKing = 'B'
  | x == WhiteKing = 'W'
  | x == Special = 'x'
  | otherwise = '.'

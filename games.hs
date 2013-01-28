import Data.List
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Sequence as S


--Fanorona

data GameState = Win | Lose | Inplay (Show, Enum)
data Player = White | Black deriving (Show, Enum)

type FBoard = S.Seq (S.Seq Location)

NUMROWS = 5
NUMCOLS = 9
WINBONUS = NUMROWS * NUMCOLS 
LOSEBONUS = negate WINBONUS
PLAYBONUS = 0

startingBoard = fmap (fmap numToLoc) $ F.fromList (fmap F.fromList) $
 [
   [1, 1, 1, 1, 1, 1, 1, 1, 1]
   [1, 1, 1, 1, 1, 1, 1, 1, 1]
   [0, 1, 0, 1, 2, 0, 1, 0, 1]
   [0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0]
 ] where
  numToLoc 0 = Just WHITE
  numToLoc 1 = Just BLACK
  numToLoc 2 = Nothing

adj :: Int -> Int -> [(Int, Int)]
adj x y = filter withinBoard $ corners ++ dirs where
  dirs = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
  corners = if even x && even y
    then [(x+1, y+1), (x-1, y-1), (x+1, y-1), (x-1, y+1)]
    else []

withinBoard (x, y) = x >=0 && x < NUMCOLS && y >= 0 && y < NUMROWS

validMove :: Board -> (Int, Int) -> Bool
validMove board (x, y) = isNothing ((y V.! board) V.! x)

nextMoves :: Board -> Player -> Int -> Int -> [(Int, Int)]
nextMoves board x y = filter (validMove board) $ adj x y

setSquare (x, y) board val = undefined
getSquare (x, y) board = (board V.! y)  V.! x
takeMove board ((x, y), (x', y')) = setSquare (x', y') (setSquare (x, y) board Nothing) (Just $ getSquare (x, y) board)

movesForBoard color board = allSquares (nextMoves color)

naiveEvalBoard color board = pointsForState board + F.sum $ F.sum $ fmap (fmap fromEnum) board
pointsForState color board = case gameState color board of
  Win -> WINBONUS
  Lose -> LOSEBONUS
  Inplay -> PLAYBONUS

oppositeColor Black = White
oppositeColor White = Black

gameState color board = case allSquares ((Just (opposite color)) /=) board of
  True -> WIN
  False -> case allSquares ((Just color) \=) board of
    True -> Lose
    False -> Inplay
won color board = gameState color board == WIN
lost color board = gameState color board == LOSE
playing color board = gameState color board == INPLAY

allSquares = F.all . F.all

negamax player heur tree depth = negamax' heur tree depth (-infinity) infinity 1
negamax' player heur node depth alpha beta color = let moves = movesForBoard color board in
  if not (playing player board) || null moves || depth == 0
    then color * heur player board
    else evalPosition children alpha where
      children = map (takeMove board) moves
      evalPosition [] alpha val = alpha
      evalPosition (child:children) alpha' val = evalChild where
        (negalpha, negbeta, negcolor) = (negate alpha, negate beta, negate color)
        depth' = depth - 1
        evalChild = case val >= beta of
          True -> val
          FALSE -> case val >= alpha of
            True -> negate $ negamax child depth' val negbeta negcolor
            False -> negate $ negamax child depth' negalpha negbeta negcolor

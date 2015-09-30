--No nulls
--No patching because not . syntax
--No optional arguments
--No overloading
--No generics (because inference)
--No polymorphism needed
--Can use hashes, but in an easy way
--No design patterns
--Change rows to count upwards

import Text exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Debug exposing (..)
import List exposing (..)
import Array exposing (..)
import Keyboard
import Signal exposing (..)
import String exposing (toInt, fromChar)
import Char exposing (fromCode)
import Maybe exposing (andThen)

----- Types -----

type Piece = Red | Black | Empty

type alias Board = Array (Array Piece)

type alias Model = 
  { board : Board
  , turn : Piece
  , win : Piece  
  } 

--Minimax types
type ColumnChoice = Int | None
type alias PotentialMove =
  { column : Maybe Int
  , score : Float
  }

-----Constants-----

width = 7
height = 6

padding = 20
pieceSize = 40

boardHeight = 700
boardWidth = boardHeight * (width / height)

availableHeight : Float
availableHeight = boardHeight - (2 * padding) - 2 * pieceSize

availableWidth : Float
availableWidth = boardWidth - (2 * padding) - 2 * pieceSize

minimaxLookAhead = 4

-----Initial State-----

startingModel : Model
startingModel = 
  { board = blankBoard
  , turn = Red
  , win = Empty
  }

blankBoard : Board
blankBoard = Array.repeat height (Array.repeat width Empty)

----- View -----

view : Model -> Element
view model =
  let board = [bg] ++ pieces model.board
  in
  collage (truncate boardWidth) boardHeight board  

-- Draw Pieces --
pieces : Board -> List Form
pieces board =
  let formBoard = toList board |> List.indexedMap (\rowNum row -> 
                    toList row |> List.indexedMap (\colNum piece -> 
                      (drawPiece piece (toFloat rowNum) (toFloat colNum)) 
                    )
                  )
  in
    formBoard |> concatMap identity

drawPiece : Piece -> Float -> Float -> Form
drawPiece piece row column =
  let color = case piece of
                Red -> Color.red
                Black -> Color.black
                Empty -> Color.white
  in drawCircle color row column

-- Draw the background --
bg = 
  rect boardWidth boardHeight |> filled blue 

-- Draw Circles --
drawCircle : Color -> Float -> Float -> Form
drawCircle color row column = circle pieceSize |> filled color |> move (translate column row)

translate : Float -> Float -> (Float,Float)
translate column row =
    (translateX column,translateY row)

translateX : Float -> Float
translateX column = 
  let startLeft = -(availableWidth / 2)
      gap = availableWidth / (width - 1)
  in 
  startLeft + (column * gap)

translateY : Float -> Float
translateY row = 
  let startTop = availableHeight / 2
      gap = availableHeight / (height - 1)
  in
  startTop - (row * gap)

----- Update -----

update : Int -> Model -> Model
update keyCode model =
  if not (model.win == Empty) then model --don't add piece if game is over
  else
    let playerTurnResult = takeTurnPlayer keyCode model
    in
    case playerTurnResult of
      Nothing -> model --illegal move
      Just model -> if not (model.win == Empty)
                    then model --player wins
                    else case takeTurnComputer model of
                           Nothing -> model --full board tie
                           Just model -> model

takeTurnPlayer : Int -> Model -> Maybe Model
takeTurnPlayer keyCode model =
  let keyNum = keyNumber keyCode
  in
  case keyNum of
    Err string -> Nothing
    Ok keyNum -> takeTurnIfValid model (keyNum - 1)

keyNumber : Int -> Result String Int
keyNumber keyCode =
  keyCode |> fromCode
          |> fromChar 
          |> toInt 
  
takeTurnIfValid : Model -> Int -> Maybe Model
takeTurnIfValid model col =
  let row = rowIfCanAddColumn model.board col
  in 
  case row of
      Just row -> Just (addPiece model row col)
      Nothing -> Nothing

rowIfCanAddColumn : Board -> Int -> Maybe Int
rowIfCanAddColumn board col = 
  if | col > width -> Nothing 
     | otherwise -> lowestEmptyRow board col

lowestEmptyRow : Board -> Int -> Maybe Int
lowestEmptyRow board col = 
  board |> Array.map (getWithDefault col Empty) --replace with col method
        |> emptyIndex  

emptyIndex : Array Piece -> Maybe Int
emptyIndex column = 
  let count = column |> 
              Array.foldl (\index item -> 
                if index == Empty then item + 1 else item
              ) 0
  in
  case count of
    0 -> Nothing
    otherwise -> Just (count - 1)

addPiece : Model -> Int -> Int -> Model --use tuple
addPiece model row column = 
  let newBoard = addPieceToBoard model.board model.turn row column
  in 
  { board = newBoard  
  , turn = opponent model.turn
  , win = checkWin newBoard model.turn
  }

addPieceToBoard : Board -> Piece -> Int -> Int -> Board
addPieceToBoard board piece row column =  
  let oldRow = getWithDefault row (fromList [Empty]) board
      newRow = Array.set column piece oldRow
  in
  Array.set row newRow board
  
getWithDefault : Int -> a -> Array a -> a
getWithDefault index default array =
  array |> get index |> Maybe.withDefault default

opponent : Piece -> Piece
opponent piece = if piece == Red then Black else Red

checkWin : Board -> Piece -> Piece
checkWin board piece =
  if (checkWinHorizontal board piece) || (checkWinVertical board piece)
  then piece
  else Empty

checkWinHorizontal : Board -> Piece -> Bool
checkWinHorizontal board piece =
  checkNested board piece 

checkNested : Board -> Piece -> Bool
checkNested board piece =
  board |> Array.foldl (\row win -> if win == True 
                                    then True 
                                    else checkWinArray row piece) False

checkWinArray : Array Piece -> Piece -> Bool
checkWinArray array piece = 
  let count = array |> Array.foldl (\p count -> if | count == 4 -> 4
                                                   | p == piece -> count + 1
                                                   | otherwise -> 0) 0
  in
  count == 4

checkWinVertical : Board -> Piece -> Bool
checkWinVertical board piece =
  let columns = fromList [0..width] |> Array.map (getColumn board)
  in
  checkNested columns piece

getColumn : Board -> Int -> Array Piece
getColumn board col = 
  board |> Array.map (getWithDefault col Empty)

----- Computer -------

takeTurnComputer : Model -> Maybe Model
takeTurnComputer model =
  let col = minimax model.board model.turn
  in
  col `andThen` getRow model

getRow : Model -> Int -> Maybe Model
getRow model col =
  (lowestEmptyRow model.board col) `andThen` (\row -> Just (addPiece model row col))

minimax : Board -> Piece -> Maybe Int
minimax board piece = 
  let potentialMove = evaluatePosition board 0 piece piece
  in potentialMove `andThen` (.column)

evaluatePosition : Board -> Int -> Piece -> Piece -> Maybe PotentialMove
evaluatePosition board depth evaluatingPiece movingPiece =
  let score = positionScore board evaluatingPiece
  in
  if | not (score == 0) -> potentialMove Nothing score
     | depth > minimaxLookAhead -> potentialMove Nothing score
     | otherwise -> bestMove board depth evaluatingPiece movingPiece

bestMove : Board -> Int -> Piece -> Maybe PotentialMove
bestMove board piece board depth evaluatingPiece movingPiece =
  let possibleNexts = possibleNextMoves board movingPiece
      possibleMoves = [
        { column = Just 2
        ,  score = 1
        },
        { column = Nothing
        , score = 0
        }
      ]
      --possibleMoves = possibleNexts |> map (\newBoard -> 
      --  evaluatePosition newBoard (depth + 1) evaluatingPiece (opponent movingPiece)
      --)
  in
  possibleMoves |> maxBy (\move -> abs move.score)

possibleNextMoves : Board -> Piece
possibleNextMoves board movingPiece =
  [0..width - 1] |> map (\col ->

  ) 

positionScore : Board -> Piece -> Float
positionScore board piece =
  let nextPiece = opponent piece
      pieceIfWon = checkWin board piece
      nextPieceIfWon = checkWin board nextPiece
  in
  if | pieceIfWon == piece -> 1
     | nextPieceIfWon == nextPiece -> -1
     | otherwise -> 0

potentialMove : Maybe Int -> Float -> Maybe PotentialMove
potentialMove col score =
  Just { column = col
       , score = score
       }

maxBy : (a -> comparable) -> List a -> Maybe a
maxBy f list =
  list |> foldl (\item max ->
    case max of
      Nothing -> item
      Just value -> if |(f value) > (f item) -> max
                       |(f value) == (f item) -> max
                       |(f value) < (f item) -> item
  ) Nothing
----def minimax(board, max_depth = 3, depth = 0, active_piece = 'R', moving_piece = 'R')
----  position_score = position_score(board, active_piece)
----  return position_score if position_score != 0
----  return 0 if depth > max_depth
----  positions = possible_next_boards(board, moving_piece).map do |board| 
----    minimax(board, max_depth, depth + 1, active_piece, opponent(moving_piece))
----  end
----  return 0 if positions.empty?
----  positions.max_by { |score| score.abs }
----end

----def position(board, score)
----  {
----    board: board,
----    score: score
----  }
----end

----def possible_next_boards(board, piece)
----  Hamster.interval(0, board.first.length - 1).map do |n|
----    add_piece_to_column(board, n, piece)
----  end.compact
----end



----- Main -----

main : Signal Element
main = view <~ gameState

gameState : Signal Model
gameState = foldp update startingModel Keyboard.presses


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

----- Types -----



type Piece = Red | Black | Empty

type alias Board = Array (Array Piece)

type alias Model = 
  { board : Board
  , turn : Piece
  , win : Piece  
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
  let key = keyCode 
              |> fromCode
              |> fromChar
              |> toInt 
  in 
  case key of
      Ok value -> addToColumn model (value - 1)
      Err msg -> model

addToColumn : Model -> Int -> Model
addToColumn model col = 
  let row = if | col > width -> Nothing 
               | not (model.win == Empty) -> Nothing --don't add piece if game is over
               | otherwise -> lowestEmptyRow model.board col
  in 
  case row of
    Just value -> addPiece model value col
    Nothing -> model

lowestEmptyRow : Board -> Int -> Maybe Int
lowestEmptyRow board col = 
  board |> Array.map (getWithDefault col Empty) 
        |> emptyIndex  

emptyIndex : Array Piece -> Maybe Int
emptyIndex column = 
  let count = column |> 
              Array.foldl (\index item -> if index == Empty then item + 1 else item) 0
  in
  case count of
    0 -> Nothing
    otherwise -> Just (count - 1)

addPiece : Model -> Int -> Int -> Model
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

----- Main -----

main : Signal Element
main = view <~ gameState

gameState : Signal Model
gameState = foldp update startingModel Keyboard.presses


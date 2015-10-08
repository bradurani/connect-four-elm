import Graphics.Collage exposing (..)
import Color exposing (..)
import Array exposing  (..)
import List exposing (..)
import Graphics.Element exposing (..)
import Basics exposing (..)
import Debug exposing (..)
import Mouse exposing (..)
import Html exposing (..)
import Signal exposing (..)

width = 7
height = 6

pieceWidth = 40
padding = 10

availableHeight : Int
availableHeight = screenHeight - (2 * padding) - 2 * pieceWidth

availableWidth : Int
availableWidth = screenWidth - (2 * padding) - 2 * pieceWidth

screenWidth = 700 * (7 // 6)
screenHeight = 700

type alias Board = Array (Array Piece)
type Piece = Red | Black | Empty

startingBoard : Board
startingBoard = 
  Array.repeat height (Array.repeat width Empty)

view : Board -> Element
view board = 
  collage screenWidth screenHeight ([bg] ++ (drawState board))

drawState : Board -> List Form
drawState board =
  let nestedShapes = toList board |> List.indexedMap (\rowNum row -> 
                        toList row |> List.indexedMap (\colNum piece -> 
                          drawPiece piece rowNum colNum 
                        )
                      )
  in nestedShapes |> concatMap identity

drawPiece : Piece -> Int -> Int -> Form
drawPiece piece row col =  
  move (toFloat (translateX col), toFloat (translateY row)) (circle pieceWidth |> filled (pieceColor piece))

pieceColor : Piece -> Color
pieceColor piece = 
  case piece of
    Empty -> Color.white
    Red -> Color.red
    Black -> Color.black

translateX : Int -> Int
translateX column = 
  let startLeft = -(availableWidth // 2)
      columnWidth = availableWidth // (width - 1)
  in 
  startLeft + (column * columnWidth)

translateY : Int -> Int
translateY row = 
  let startTop = availableHeight // 2
      columnHeight = availableHeight // (height - 1)
  in
  startTop - (row * columnHeight)

addPiece : Board -> Piece -> Int -> Board
addPiece board piece columnNum =
  let row = board |> get 1 
  in case row of
    Nothing -> startingBoard
    Just row -> let newRow = setPiece row columnNum piece
                in board |> set 1 newRow 

setPiece : Array Piece -> Int -> Piece -> Array Piece
setPiece row columnNum piece=
  row |> set columnNum piece

bg : Form
bg = 
  rect (toFloat screenWidth) (toFloat screenHeight) |> filled blue

columnNum : Int -> Int
columnNum x = 1

update : (Int,Int) -> Board -> Board
update mousePos board =
  let column = fst mousePos |> columnNum
  in addPiece board Red column 

main : Signal Element
main = view <~ gameState

gameState : Signal Board
gameState = foldp update startingBoard (sampleOn Mouse.clicks Mouse.position)






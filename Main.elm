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

type alias State = { board : Board
                   , turn : Piece
                   , winner : Piece
                   }

type alias Board = Array (Array Piece)
type Piece = Red | Black | Empty


startingBoard : Board
startingBoard = 
  Array.repeat height (Array.repeat width Empty)

startingState : State
startingState =
  { board = startingBoard
  , turn = Red
  , winner = Empty
  }

------VIEW--------

view : State -> Element
view state = 
  collage screenWidth screenHeight ([bg] ++ (drawState state.board))

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
  (drawCircle piece) |> move (toFloat (translateX col), toFloat (translateY row)) 

drawCircle : Piece -> Form
drawCircle piece =
  (circle pieceWidth |> filled (pieceColor piece))

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

bg : Form
bg = 
  rect (toFloat screenWidth) (toFloat screenHeight) |> filled blue

---UPDATE LOOP MAIN LOGIC-----

update : (Int,Int) -> State -> State
update mousePos state =
  let maybeUpdatedBoard = updatePiece mousePos state.turn state.board
  in case maybeUpdatedBoard of
       Nothing -> state --illegal click or full column
       Just newBoard -> { state | board <- newBoard
                                , turn <- opponent state.turn 
                                , winner <- detectWinner newBoard }

opponent : Piece -> Piece
opponent piece =
  case piece of 
    Red -> Black
    Black -> Red


updatePiece : (Int, Int) -> Piece -> Board -> Maybe Board
updatePiece mousePos piece board =
  let maybeColumn = mousePos |> columnNum
  in case maybeColumn of
  Nothing -> Nothing
  Just columnNum -> Just (addPiece board piece columnNum)

addPiece : Board -> Piece -> Int -> Board
addPiece board piece columnNum =
  let maybeRow = getBottomRow board columnNum
  in case maybeRow of
    Nothing -> startingBoard
    Just rowNum -> board |> addPieceAt rowNum columnNum piece

addPieceAt : Int -> Int -> Piece -> Board -> Board
addPieceAt rowNum colNum piece board =
  let row = unsafeGetArray rowNum board
      newRow = setPiece row colNum piece
  in board |> set rowNum newRow 

setPiece : Array Piece -> Int -> Piece -> Array Piece
setPiece row columnNum piece =
  row |> set columnNum piece

columnNum : (Int, Int) -> Maybe Int
columnNum (x, y) =
  let
    validY = y > 0 && y <= screenHeight
    validX = x > 0 && x <= screenWidth
  in
  if validX && validY 
  then Just (colMap x)
  else Nothing

colMap : Int -> Int
colMap mouseX =
  let columnWidth = screenWidth // width
  in (mouseX // columnWidth)

getBottomRow : Board -> Int -> Maybe Int
getBottomRow board columnNum =
  let column = board |> Array.map (unsafeGetPiece columnNum)
      sum = column |> Array.foldl (\piece sum -> if piece == Empty 
                                                 then sum 
                                                 else sum + 1) 0
      row = (height - sum) - 1
  in if row < 0 || row > (height - 1) 
     then Nothing
     else Just row

unsafeGetArray : Int -> Board -> Array Piece
unsafeGetArray index board =
  let maybeArray = get index board
  in case maybeArray of 
     Nothing -> fromList []
     Just a -> a

unsafeGetPiece : Int -> Array Piece -> Piece
unsafeGetPiece index array =
  let maybeInt = get index array
  in case maybeInt of 
     Nothing -> Empty
     Just i -> i

detectWinner : Board -> Piece
detectWinner board piece =
  let winner = detectRowWinners board piece
  in
  if winner != Empty
  then winner
  else detectColumnWinners board piece

detectRowWinners : Board -> Piece -> Piece
detectRowWinners board piece =
  board |> fold (\row winner -> let rowWinner = detectArrayWinners row piece
                                in
                                if (rowWinner == Empty)
                                then winner
                                else rowWinner) Empty

detectColumnWinners : Board -> Piece -> Piece
detectColumnWinners board piece =
  let columns = [0..width - 1] |> map (getColumn board) 
  in
  board |> fold (\row winner -> let rowWinner = detectArrayWinners row piece
                                in
                                if (rowWinner == Empty)
                                then winner
                                else rowWinner) Empty

getColumn : Board -> Int -> Array Piece
getColumn board columnNum =
  board |> map (unsafeGet colNum)

detectArrayWinners : Array Piece -> Piece -> Piece
detectArrayWinners array piece =
let count = array |> foldl (\p count -> if| count == 4 -> count
                                           | piece == p -> count + 1
                                           otherwise -> 0) 0
in if count == 4
   then piece
   else Empty


----GAME STATE----

gameState : Signal State
gameState = foldp update startingState (sampleOn Mouse.clicks Mouse.position)

main : Signal Element
main = view <~ gameState


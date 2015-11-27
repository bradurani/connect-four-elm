--No nulls
--No patching because not . syntax
--No optional arguments
--No overloading
--No generics (because inference)
--No polymorphism needed
--Can use hashes, but in an easy way
--No design patterns
--Change rows to count upwards

--Change to use xs

import Text exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Debug exposing (..)
import List exposing (..)
import Array exposing (..)
import Mouse
import Signal exposing (..)
import String exposing (toInt, fromChar)
import Char exposing (fromCode)
import Maybe exposing (andThen)
import Random exposing (..)
import Time exposing (millisecond)

----- Types -----

type Piece = Red | Black | Empty

type alias Board = Array (Array Piece)

type alias Model =
  { board : Board
  , turn : Piece
  , win : Piece
  , seed : Seed
  , moveNum : Int
  }

-----Constants-----

width = 7
height = 6

padding = 20
pieceSize = 40

boardHeight = 700
boardWidth = boardHeight * (width // height)

availableHeight : Int
availableHeight = boardHeight - (2 * padding) - 2 * pieceSize

availableWidth : Int
availableWidth = boardWidth - (2 * padding) - 2 * pieceSize

minimaxLookAhead = 3

seedVal = 43

-----Initial State-----

startingModel : Model
startingModel = 
  { board = blankBoard
  , turn = Red
  , win = Empty
  , seed = Random.initialSeed seedVal
  , moveNum = 0
  }

blankBoard : Board
blankBoard = Array.repeat height (Array.repeat width Empty)

----- Util ----

(?) : Maybe a -> a -> a
(?) maybe default =
  Maybe.withDefault default maybe

infixr 9 ?

randomInt : Int -> Seed -> (Int, Seed)
randomInt max seed = 
  generate (int 0 max) seed

----- View -----

view : Model -> Element
view model =
  let board = [bg] ++ pieces model.board
  in
  collage boardWidth boardHeight board  

-- Draw Pieces --
pieces : Board -> List Form
pieces board =
  let formBoard = toList board |> List.indexedMap (\rowNum row -> 
                    toList row |> List.indexedMap (\colNum piece -> 
                      drawPiece piece rowNum colNum 
                    )
                  )
  in
    formBoard |> concatMap identity

drawPiece : Piece -> Int -> Int -> Form
drawPiece piece row column =
  let color = case piece of
                Red -> Color.red
                Black -> Color.black
                Empty -> Color.white
  in drawCircle color row column

-- Draw the background --
bg = 
  rect (toFloat boardWidth) (toFloat boardHeight) |> filled blue 

-- Draw Circles --
drawCircle : Color -> Int -> Int -> Form
drawCircle color row column = 
  let coords = translate column row
      floatCoords = (toFloat (fst coords), toFloat (snd coords))
  in
  circle pieceSize |> filled color |> move floatCoords

translate : Int -> Int -> (Int,Int)
translate column row =
    (translateX column,translateY row)

translateX : Int -> Int
translateX column = 
  let startLeft = -(availableWidth // 2)
      gap = availableWidth // (width - 1)
  in 
  startLeft + (column * gap)

translateY : Int -> Int
translateY row = 
  let startTop = availableHeight // 2
      gap = availableHeight // (height - 1)
  in
  startTop - (row * gap)

----- Update -----

update : (Int, Int) -> Model -> Model
update mousePosition model =
  if not (model.win == Empty) 
  then model --don't add piece if game is over
  else
    let playerTurnResult = takeTurnPlayer mousePosition model
    in
    case playerTurnResult of
      Nothing -> model --illegal move
      Just afterPlayerModel -> 
        let newModel = Debug.watchSummary "model" .moveNum afterPlayerModel
        in
        if not (newModel.win == Empty)
        then newModel --player wins
        else case takeTurnComputer newModel of
               Nothing -> newModel --full board tie
               Just afterComputerModel -> afterComputerModel

columnNumber : (Int, Int) -> Maybe Int
columnNumber position = 
  let
    x = fst position
    y = snd position
  in 
    if x < 0 || x > boardWidth then
      Nothing
    else if y < 0 || y > boardHeight then
      Nothing
    else
      Just (translateMouseCol x)

translateMouseCol : Int -> Int
translateMouseCol x =
  let delta = boardWidth // width
  in x // delta

takeTurnPlayer : (Int, Int) -> Model -> Maybe Model
takeTurnPlayer mousePosition model =
  let col = columnNumber mousePosition
  in
  case col of
    Nothing -> Nothing
    Just col -> takeTurnIfValid model col
  
takeTurnIfValid : Model -> Int -> Maybe Model
takeTurnIfValid model col =
  let row = rowIfCanAddColumn model.board col
  in 
  case row of
      Just row -> Just (addPiece model row col)
      Nothing -> Nothing

rowIfCanAddColumn : Board -> Int -> Maybe Int
rowIfCanAddColumn board col = 
  if col > width then
    Nothing
  else
    lowestEmptyRow board col

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
  let
    newBoard = addPieceToBoard model.board model.turn row column
  in
    {
      model | 
      board = newBoard
    , turn = opponent model.turn
    , win = checkWin newBoard model.turn
    , moveNum = model.moveNum + 1
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

getFromBoard : Int -> Int -> Board -> Piece
getFromBoard rowNum columnNum board =
  let row = getWithDefault columnNum (fromList [Empty]) board
  in getWithDefault rowNum Empty row

opponent : Piece -> Piece
opponent piece = if piece == Red then Black else Red

checkWin : Board -> Piece -> Piece
checkWin board piece =
  if (checkWinHorizontal board piece) 
  || (checkWinVertical board piece) 
  || (checkWinDiagonal board piece)
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
  let
      count = array |>
                Array.foldl (\p count ->
                  if count == 4 then
                     4
                  else if p == piece then
                     count + 1
                  else
                     0
                 ) 0
  in
  count == 4

checkWinVertical : Board -> Piece -> Bool
checkWinVertical board piece =
  let columns = fromList [0..width] |> Array.map (getColumn board)
  in
  checkNested columns piece

checkWinDiagonal : Board -> Piece -> Bool
checkWinDiagonal board piece =
     (checkNested ((yDiagonalsDownRight board)) piece) 
  || (checkNested ((xDiagonalsDownRight board)) piece)
  || (checkNested ((yDiagonalsDownLeft board)) piece)
  || (checkNested ((xDiagonalsDownLeft board)) piece)

yDiagonalsDownRight : Board -> Board
yDiagonalsDownRight board =
  fromList [0..height - 1] |> 
    Array.map (\y -> diagonal (+) 0 y board)

xDiagonalsDownRight : Board -> Board
xDiagonalsDownRight board =
  fromList [0..width - 1] |> 
    Array.map (\x -> diagonal (+) x 0 board)

yDiagonalsDownLeft : Board -> Board
yDiagonalsDownLeft board =
  fromList [0..height - 1] |> 
    Array.map (\y -> diagonal (-) (width - 1) y board)

xDiagonalsDownLeft : Board -> Board
xDiagonalsDownLeft board =
  fromList [0..width - 1] |> 
    Array.map (\x -> diagonal (-) x 0 board)

diagonal : (Int -> Int -> Int) -> Int -> Int -> Board -> Array Piece
diagonal operator x y board =
  let startingRecord = { array = fromList []
                       , n = 0 }
      record = board |> Array.foldl (\row results ->
                                      if x < 0 || x >= width || y < 0 || y >= height
                                      then results
                                      else
                                      { results |
                                        array = push (getFromBoard (x `operator` results.n) (y + results.n) board) results.array
                                      , n = results.n + 1 }) startingRecord
  in record.array



getColumn : Board -> Int -> Array Piece
getColumn board col = 
  board |> Array.map (getWithDefault col Empty)

----- Computer -------

takeTurnComputer : Model -> Maybe Model
takeTurnComputer model = 
  let lookAheadModel = evaluatePosition model 0
  in
  Maybe.map (resetWin model.turn) lookAheadModel --the algorithm marks a win even if it's several moves ahead
                                                  --so we need to reset it to Empty so play continues

evaluatePosition : Model -> Int -> Maybe Model
evaluatePosition model depth =
  if not (model.win == Empty) then
     Just model
  else if depth == minimaxLookAhead then
     Just model
  else
     bestMove model depth

bestMove : Model -> Int -> Maybe Model
bestMove model depth =
  let possibleNexts = possibleNextMoves model
      endingPositions = possibleNexts  
                          |> List.map (\m -> (m, evaluatePosition m (depth + 1)))
                          |> filterNothingModelTuples
                          |> List.map (\m -> copyWin m)
  in
  endingPositions |> winner model.turn model.seed |> Maybe.map fst

possibleNextMoves : Model -> List Model
possibleNextMoves model =
  [0..width - 1] |> List.map (takeTurnIfValid model)
                 |> filterNothingModels

winner : Piece -> Seed -> List (Model, Model) -> Maybe (Model, Model)
winner piece seed list =
  let f p = List.filter(snd >> .win >> (==) p)
      winners = list |> f piece  
      losers = list |> f (opponent piece)
      draws = list |> f Empty
  in 
  if List.length winners > 0 then
     Just (sampleModelTuple winners seed)
  else if List.length draws > 0 then
     Just (sampleModelTuple draws seed)
  else if List.length losers > 0 then
     Just (sampleModelTuple losers seed)
  else
     Nothing --draw, board full

copyWin : (Model, Model) -> (Model, Model)
copyWin (a, b) =
  ({ a | win = b.win}, b) 

filterNothingModels : List (Maybe Model) -> List Model
filterNothingModels list =
  list |> List.filter ((==) Nothing >> not)
       |> List.map (\m -> m ? startingModel)

filterNothingModelTuples : List ((Model, Maybe Model)) -> List (Model, Model)
filterNothingModelTuples list =
  list |> List.filter (snd >> (==) Nothing >> not)
       |> List.map (\t -> (fst t, (snd t) ? startingModel))

resetWin : Piece -> Model -> Model
resetWin piece lookAheadModel =
  let win = checkWin lookAheadModel.board piece
  in
  if win == piece 
  then lookAheadModel
  else { lookAheadModel | win = Empty }

sampleModelTuple : List (Model, Model) -> Seed -> (Model, Model)
sampleModelTuple list seed =
  let ((a, b), newSeed) = sample list seed (startingModel, startingModel)
  in ({ a | seed = newSeed},
      { b | seed = newSeed})

sample : List a -> Seed -> a -> (a, Seed)
sample list seed default =
  let rand = randomInt ((List.length list) - 1) seed
      val = (get (fst rand) (fromList list)) ? default
  in
  (val, (snd rand))
  

----- Main -----

main : Signal Element
main = Signal.map view gameState

gameState : Signal Model
gameState = foldp update startingModel (sampleOn Mouse.clicks Mouse.position)


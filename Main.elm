import Text exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Debug exposing (..)
import List exposing (..)

--main =
--  StartApp.start { model = model, view = view, update = update }

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

main = 
  collage (truncate boardWidth) boardHeight ([board] ++ circles)

board = 
  rect boardWidth boardHeight |> filled blue

circles = concatMap intoRows [0..height-1]
intoRows row = map (drawCircle row) [0..width-1] 

drawCircle : Float -> Float -> Form
drawCircle row column = circle pieceSize |> filled white |> move (translate column row)

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

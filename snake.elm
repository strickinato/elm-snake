import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Signal
import Text
import Time exposing (..)
import Window
import List
import Array exposing (..)

-----------------------------------------------------------
--------- SIGNALS -----------------------------------------
-----------------------------------------------------------
delta =
  Signal.map inSeconds (fps gameSpeed)

input : Signal Input
input =
  Signal.map2 Input
    Keyboard.arrows
    delta

type alias Input =
  { direction : Direction
  , timeDelta : Float
  }

main =
  Signal.map2 view Window.dimensions gameState


-----------------------------------------------------------
--------- STATE -------------------------------------------
-----------------------------------------------------------
gameState : Signal GameState
gameState =
  Signal.foldp stepGame defaultGame input

boardSize = 800  -- `pixels` squared
gameSpeed = 1   -- updates per second

type alias Direction = { x : Int, y : Int }

type alias Square = (Int, Int)

type alias Snake =
  {direction:Direction, parts:List(Square)}

type alias GameState =
  {snake:Snake}


defaultSnake =
  { direction = { x = 1, y = 0 }
  , parts = [(0,0),(1,0),(2,0)]
  }


defaultGame : GameState
defaultGame =
  { snake  = defaultSnake }


-----------------------------------------------------------
--------- UPDATE ------------------------------------------
-----------------------------------------------------------
stepGame : Input -> GameState -> GameState
stepGame {timeDelta, direction} ({snake} as gameState) =
  let
    newSnake = updateSnake direction snake
  in
    { gameState |
        snake <- newSnake
      }


updateSnake : Direction -> Snake -> Snake
updateSnake direction snake =
  let newDirection =
    if direction == { x = 0, y = 0 }
      then snake.direction
      else direction

      newParts = updateParts snake.direction snake.parts
  in
    { snake |
        direction <- newDirection
      , parts   <- newParts
      }


updateParts : Direction -> List Square -> List Square
updateParts direction parts =
  case parts of
    segment :: []       -> segment :: []
    segment :: segments -> move direction segment :: segment :: init segments


move : Direction -> Square -> Square
move direction square =
  ( fst square + direction.x
  , snd square + direction.y
  )


-----------------------------------------------------------
--------- DISPLAY -----------------------------------------
-----------------------------------------------------------
view : (Int,Int) -> GameState -> Element
view (w,h) {snake} =
  flow down [(show snake), (displayBoard boardSize)]

displayBoard : Int -> Element
displayBoard size =
    collage size size
      [ filled (Color.rgb 0 0 0) (square (toFloat (size // 2))) ]

-----------------------------------------------------------
--------- UTILITY -----------------------------------------
-----------------------------------------------------------
init : List(Square) -> List(Square)
init squares =
  case squares of
    square :: []   -> []
    square :: squares -> square :: init squares
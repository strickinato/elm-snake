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
--------- CONFIG -----------------------------------------
-----------------------------------------------------------
gameSize = 1000    -- total gamesize squared
pixels   = 3     -- pixels in game
gameSpeed = 40    -- updates per second
startingLength = 100

-----------------------------------------------------------
--------- SIGNALS -----------------------------------------
-----------------------------------------------------------
delta =
  Signal.map inSeconds (fps gameSpeed)

input : Signal Input
input =
  Signal.merge
    (Signal.map Dir Keyboard.arrows)
    (Signal.map Delta delta)

type Input
  = Dir Direction
  | Delta Float


main =
  Signal.map2 view Window.dimensions gameState


-----------------------------------------------------------
--------- STATE -------------------------------------------
-----------------------------------------------------------
gameState : Signal GameState
gameState =
  Signal.foldp stepGame defaultGame input


type alias Direction = { x : Int, y : Int }

type alias Square = (Int, Int)

type alias Snake =
  {direction:Direction, parts:List(Square)}

type alias GameState =
  {snake:Snake, gameSize:Int}


defaultSnake =
  { direction = { x = 1, y = 0 }
  , parts = defaultParts startingLength
  }

defaultParts : Int -> List Square
defaultParts length =
  if | length == 0 -> []
     | otherwise ->
        (length - 1 - startingLength, 0) :: defaultParts (length - 1)

defaultGame : GameState
defaultGame =
  { snake = defaultSnake
  , gameSize = gameSize
  }


-----------------------------------------------------------
--------- UPDATE ------------------------------------------
-----------------------------------------------------------
stepGame : Input -> GameState -> GameState
stepGame input ({snake, gameSize} as gameState) =
  let
    newSnake = updateSnake input snake
  in
    { gameState |
        snake <- newSnake
      , gameSize <- gameSize
    }

updateSnake : Input -> Snake -> Snake
updateSnake input snake =
  case input of
    Dir direction ->
      let newDirection =
        if direction == { x = 0, y = 0 }
          then snake.direction
          else direction
      in
        { snake | direction <- newDirection }

    Delta float ->
      let
        newParts = updateParts snake.direction snake.parts
      in
        { snake | parts <- newParts }


updateParts : Direction -> List Square -> List Square
updateParts direction parts =
  case parts of
    segment :: []       -> segment :: []
    segment :: segments -> step direction segment :: segment :: init segments


step : Direction -> Square -> Square
step direction square =
  ( fst square + direction.x
  , snd square + direction.y
  )


-----------------------------------------------------------
--------- DISPLAY -----------------------------------------
-----------------------------------------------------------
view : (Int,Int) -> GameState -> Element
view (w,h) {snake, gameSize} =
  collage w h
      ((filled (Color.rgb 0 0 0) (square (toFloat (gameSize)))) :: (renderSnake snake))

renderSnake : Snake -> List Form
renderSnake {direction, parts} =
  List.map placeSegment parts

placeSegment : Square -> Form
placeSegment (x, y) =
  move ((toFloat (x * pixels)), (toFloat (y * pixels))) (filled (Color.rgb 255 0 255) (square (toFloat (pixels))))
-----------------------------------------------------------
--------- UTILITY -----------------------------------------
-----------------------------------------------------------
init : List(Square) -> List(Square)
init squares =
  case squares of
    square :: []      -> []
    square :: squares -> square :: init squares
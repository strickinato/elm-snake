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
--------- CONFIG ------------------------------------------
-----------------------------------------------------------
boardSize        = 1000
scale            = 3
gameSpeed        = 40
startingLength   = 100

boardColor       = Color.rgb 0 0 0
snakeColor       = Color.rgb 255 0 255


-----------------------------------------------------------
--------- SIGNALS -----------------------------------------
-----------------------------------------------------------
main =
  Signal.map2 view Window.dimensions gameState


input : Signal Input
input =
  Signal.merge
    (Signal.map Dir Keyboard.arrows)
    (Signal.map Delta delta)


delta : Signal Float
delta =
  Signal.map inSeconds (fps gameSpeed)


-----------------------------------------------------------
--------- STATE -------------------------------------------
-----------------------------------------------------------
gameState : Signal GameState
gameState =
  Signal.foldp stepGame defaultGame input

type Input
  = Dir Direction
  | Delta Float

type alias Direction = { x : Int, y : Int }

type alias Square = (Int, Int)

type alias Snake =
  {direction:Direction, parts:List(Square)}

type alias GameState =
  {snake:Snake, boardSize:Float}


defaultSnake : Snake
defaultSnake =
  { direction = { x = 1, y = 0 }
  , parts = defaultParts startingLength
  }


defaultParts : Int -> List Square
defaultParts length =
  if length == 0
    then []
    else (length - 1 - startingLength, 0) :: defaultParts (length - 1)


defaultGame : GameState
defaultGame =
  { snake = defaultSnake
  , boardSize = boardSize
  }


-----------------------------------------------------------
--------- UPDATE ------------------------------------------
-----------------------------------------------------------
stepGame : Input -> GameState -> GameState
stepGame input ({snake, boardSize} as gameState) =
  let
    newSnake = updateSnake input snake
  in
    { gameState |
        snake <- newSnake
      , boardSize <- boardSize
    }


updateSnake : Input -> Snake -> Snake
updateSnake input snake =
  case input of
    Dir direction ->
      let
        newDirection =
          if direction == { x = 0, y = 0 }
            then snake.direction
            else direction
      in
        { snake | direction <- newDirection }

    Delta _ ->
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
view (w,h) {snake, boardSize} =
  collage w h
      <| renderBoard boardSize :: renderSnake snake


renderBoard : Float -> Form
renderBoard size =
  filled boardColor
    <| square size


renderSnake : Snake -> List Form
renderSnake {direction, parts} =
  List.map placeSegment parts


placeSegment : Square -> Form
placeSegment (x, y) =
  move (scaled x, scaled y)
    <| filled snakeColor
    <| square (scaled 1)


-----------------------------------------------------------
--------- UTILITY -----------------------------------------
-----------------------------------------------------------
init : List Square -> List Square
init squares =
  case squares of
    square :: []      -> []
    square :: squares -> square :: init squares

scaled : Int -> Float
scaled x =
  toFloat (x * scale)


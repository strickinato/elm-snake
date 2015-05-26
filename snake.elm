import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Array exposing (..)
import Random

-----------------------------------------------------------
--------- CONFIG ------------------------------------------
-----------------------------------------------------------
boardSize        = 700
scale            = 10
frameSpeed       = 20
snakeSpeed       = 2
startingLength   = 10

boardColor       = Color.rgb 0 0 0
snakeColor       = Color.rgb 255 0 255
appleColor       = Color.rgb 0 255 255


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
  Signal.map inSeconds (fps frameSpeed)


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

type alias Apple =
  {square:Square, seed:Random.Seed}

type alias GameState =
  { snake:Snake
  , boardSize:Float
  , score:Int
  , apple:Apple
  }

port startTime : Int

defaultSnake : Snake
defaultSnake =
  { direction = { x = 1, y = 0 }
  , parts = defaultParts startingLength }


defaultParts : Int -> List Square
defaultParts length =
  if length == 0
    then []
    else (length - 1 - startingLength, 0) :: defaultParts (length - 1)


defaultGame : GameState
defaultGame =
  { snake = defaultSnake
  , boardSize = boardSize
  , score = 0
  , apple = {square = (10, 20), seed = Random.initialSeed startTime }
  }



-----------------------------------------------------------
--------- UPDATE ------------------------------------------
-----------------------------------------------------------
stepGame : Input -> GameState -> GameState
stepGame input ({snake, score, apple} as gameState) = 
  let
    appleContact = collision snake apple

  in
    { gameState | snake <- updateSnake input snake appleContact
                , score <- if appleContact then score + 1 else score
                , apple <- if appleContact then placeApple apple else apple
                }

updateSnake : Input -> Snake -> Bool -> Snake
updateSnake input snake contact =
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
      { snake | parts <- moveParts snake.direction contact snake.parts}

moveParts : Direction -> Bool -> List Square -> List Square
moveParts direction contact parts =
  case parts of
    segment :: []       -> step direction segment :: []
    segment :: segments -> step direction segment :: segment :: if contact then segments else init segments


step : Direction -> Square -> Square
step direction square =
  ( fst square + direction.x
  , snd square + direction.y
  )

collision : Snake -> Apple -> Bool
collision snake apple =
  let 
    mouth = List.head snake.parts
  in
    case mouth of
      Just square -> square == apple.square
      Nothing -> False

placeApple : Apple -> Apple
placeApple apple =
  let
    range = boardSize // (scale * 2)
    (x, seed') = Random.generate (Random.int -10 10) apple.seed
    (y, seed'') = Random.generate (Random.int -10 10) seed'
  in
    { square = (x,y), seed = seed'' }


-----------------------------------------------------------
--------- DISPLAY -----------------------------------------
-----------------------------------------------------------
view : (Int,Int) -> GameState -> Element
view (w,h) {snake, boardSize, score, apple} =
  collage w h
      <| renderScore score :: renderBoard boardSize :: placeSegment appleColor apple.square :: renderSnake snake

renderScore : Int -> Form
renderScore score =
  move (0,(boardSize / 2) + 10)
    <| toForm (show score)
  

renderBoard : Float -> Form
renderBoard size =
  filled boardColor
    <| square size


renderSnake : Snake -> List Form
renderSnake {direction, parts} =
  List.map (placeSegment snakeColor) parts


placeSegment : Color.Color -> Square -> Form
placeSegment color (x, y) =
  move (scaled x, scaled y)
    <| filled color
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

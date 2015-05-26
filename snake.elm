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

type Chomp
  = Food
  | Tail
  | Border
  | Air

type alias Direction = { x : Int, y : Int }

type alias Square = (Int, Int)

type alias Snake =
  {direction:Direction, parts:List(Square), chomp:Chomp}

type alias Apple =
  {square:Square, seed:Random.Seed}

type alias GameState =
  { snake:Snake
  , boardSize:Float
  , score:Int
  , apple:Apple
  }

--port startTime : Int

defaultSnake : Snake
defaultSnake =
  { direction = { x = 1, y = 0 }
  , parts = defaultParts startingLength
  , chomp = Air
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
  , score = 0
  , apple = {square = (10, 20), seed = Random.initialSeed 20 }
  }



-----------------------------------------------------------
--------- UPDATE ------------------------------------------
-----------------------------------------------------------
stepGame : Input -> GameState -> GameState
stepGame input ({snake, score, apple, boardSize} as gameState) =
  case input of
    Dir direction ->
      { gameState | snake <- updateDirection direction snake }

    Delta _ ->
      let
        snakeState = checkSnake gameState
      in case snakeState of
          Air ->
            { gameState | snake <- stepSnake snake }
          Food ->
            { gameState | snake <- eatingSnake snake
                        , score <- score + 1
                        , apple <- placeApple apple
            }
          Tail ->
            gameState
          Border ->
            gameState


updateDirection : Direction -> Snake -> Snake
updateDirection ({x, y} as direction) snake =
  let
    current = snake.direction
  in
    if | direction == {x = 0, y = 0} -> snake
       | direction == {x = -current.x, y = -current.y} -> snake
       | (abs x) + (abs y) > 1 -> snake
       | otherwise -> { snake | direction <- direction }


checkSnake : GameState -> Chomp
checkSnake ({snake, score, apple} as gameState) =
  let
    snakeTail = ensureListTail snake.parts

  in
    if | collision snake apple.square -> Food
       | List.any (collision snake) snakeTail -> Tail
       | otherwise -> Air

changeSnakeDirection : Snake -> Direction -> Snake
changeSnakeDirection snake newDirection =
  { snake | direction <- newDirection }

stepSnake : Snake -> Snake
stepSnake snake =
  { snake | parts <- moveParts snake.direction snake.parts False }


eatingSnake : Snake -> Snake
eatingSnake snake =
  { snake | parts <- moveParts snake.direction snake.parts True }


moveParts : Direction -> List Square -> Bool -> List Square
moveParts direction parts eating =
  case parts of
    head :: []       -> step direction head :: []
    head :: tail -> step direction head :: head :: if eating then tail else init tail


step : Direction -> Square -> Square
step direction square =
  ( fst square + direction.x
  , snd square + direction.y
  )


placeApple : Apple -> Apple
placeApple apple =
  let
    range = boardSize // (scale * 2)
    (x, seed') = Random.generate (Random.int -10 10) apple.seed
    (y, seed'') = Random.generate (Random.int -10 10) seed'
  in
    { square = (x,y), seed = seed'' }


collision : Snake -> Square -> Bool
collision snake square =
  let 
    mouth = List.head snake.parts
  in
    case mouth of
      Just chomp -> chomp == square
      Nothing -> False

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
init : List a -> List a
init list =
  case list of
    head :: []   -> []
    head :: tail -> head :: init tail

scaled : Int -> Float
scaled x =
  toFloat (x * scale)

ensureListTail : List a -> List a
ensureListTail tail =
  let
    maybeTail = List.tail tail
  in
    case maybeTail of
      Just tail -> tail
      Nothing -> []
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
frameSpeed       = 15
startingLength   = 10
startingApples   = 25

boardColor       = Color.rgb 0 0 0
snakeColor       = Color.rgb 255 0 255
snake2Color      = Color.rgb 0 0 255
appleColor       = Color.rgb 0 255 255


-----------------------------------------------------------
--------- SIGNALS -----------------------------------------
-----------------------------------------------------------
main =
  Signal.map2 view Window.dimensions gameState


input : Signal Input
input =
  Signal.merge
    (Signal.merge
      (Signal.map Dir Keyboard.arrows)
      (Signal.map Dir2 Keyboard.wasd))
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
  | Dir2 Direction
  | Delta Float

type Chomp
  = Food
  | Tail
  | Border
  | Air

type alias Direction = { x : Int, y : Int }

type alias Square = (Int, Int)

type alias Snake =
  { direction:Direction
  , parts:List(Square)
  , chomp:Chomp
  , score:Int
  , color:Color.Color
  }

type alias Apple =
  {square:Square, seed:Random.Seed, owner:Maybe Snake}

type alias GameState =
  { snake:Snake
  , snake2:Snake
  , boardSize:Float
  , apples:List Apple
  }

port startTime : Int

defaultSnake : Int -> Color.Color -> Snake
defaultSnake dir color=
  { direction = { x = 0 , y = dir }
  , parts = List.map (\(x,y) -> (x * dir, y)) (defaultParts startingLength)
  , chomp = Air
  , score = 0
  , color = color
  }


defaultApples : Int -> List Apple
defaultApples length =
  if length == 0
    then []
    else placeApple (appleConstructor (startTime * length)) :: defaultApples (length - 1)


appleConstructor : Int -> Apple
appleConstructor x =
  {square = (x, x), seed = Random.initialSeed x, owner = Nothing }


defaultParts : Int -> List Square
defaultParts length =
  if length == 0
    then []
    else (length - 1 - startingLength, 0) :: defaultParts (length - 1)


defaultGame : GameState
defaultGame =
  { snake = defaultSnake -1 snakeColor
  , snake2 = defaultSnake 1 snake2Color
  , boardSize = boardSize
  , apples = defaultApples startingApples
  }


range : Int
range = (boardSize // (scale * 2)) - 1


placeApple : Apple -> Apple
placeApple apple =
  let
    (x, seed')  = Random.generate (Random.int -range range) apple.seed
    (y, seed'') = Random.generate (Random.int -range range) seed'
  in
    { square = (x,y), seed = seed'', owner = apple.owner}


-----------------------------------------------------------
--------- UPDATE ------------------------------------------
-----------------------------------------------------------
stepGame : Input -> GameState -> GameState
stepGame input ({snake, snake2, apples, boardSize} as gameState) =
  case input of
    Dir direction ->
      { gameState | snake <- updateSnakeDirection direction snake }

    Dir2 direction ->
      { gameState | snake2 <- updateSnakeDirection direction snake2 }

    Delta _ ->
      { gameState | snake <-  updateSnake snake gameState
                  , snake2 <- updateSnake snake2 gameState
                  , apples <- checkForSteals gameState
      }

checkForSteals : GameState -> List Apple
checkForSteals {apples, snake, snake2} =
  List.map (gettingEaten snake snake2) apples

-- If both snakes eat at the exact same time... what should happen?
gettingEaten : Snake -> Snake -> Apple -> Apple
gettingEaten snake snake2 apple =
  if | collision snake apple.square  -> stealApple snake apple
     | collision snake2 apple.square -> stealApple snake2 apple
     | otherwise                     -> apple
  

stealApple : Snake -> Apple -> Apple
stealApple snake apple =
  { apple | owner <- Just snake }

updateSnake : Snake -> GameState -> Snake
updateSnake snake gameState =
  let
    snakeState = checkSnakeStatus snake gameState

  in case snakeState of
      Air ->
        movingSnake snake
      Food ->
        eatingSnake snake
      Tail ->
        snake
      Border ->
        snake

updateSnakeDirection : Direction -> Snake -> Snake
updateSnakeDirection ({x, y} as direction) snake =
  let
    current = snake.direction
  in
    if | direction == {x = 0, y = 0} -> snake
       | direction == {x = -current.x, y = -current.y} -> snake
       | otherwise -> { snake | direction <- direction }


checkSnakeStatus : Snake -> GameState -> Chomp
checkSnakeStatus snake ({apples} as gameState) =
  let
    snakeTail = ensureListTail snake.parts

  in
    if | List.any (collision snake) (List.map (\a -> a.square) apples) -> Food
       | List.any (collision snake) snakeTail -> Tail
       | List.any (outsideBorder) snake.parts -> Border
       | otherwise -> Air

outsideBorder : Square -> Bool
outsideBorder (x, y) =
  (abs x >= abs range) || (abs y >= abs range)


movingSnake : Snake -> Snake
movingSnake snake =
  { snake | parts <- moveSnakeParts snake.direction snake.parts False }


eatingSnake : Snake -> Snake
eatingSnake snake =
  { snake | parts <- moveSnakeParts snake.direction snake.parts True }


moveSnakeParts : Direction -> List Square -> Bool -> List Square
moveSnakeParts direction parts eating =
  case parts of
    head :: []   -> step direction head :: []
    head :: tail -> step direction head :: head :: if eating then tail else init tail



step : Direction -> Square -> Square
step direction square =
  ( fst square + direction.x
  , snd square + direction.y
  )


collision : Snake -> Square -> Bool
collision snake square =
  let 
    mouth = List.head snake.parts
  in
    case mouth of
      Just chomp -> chomp == square
      Nothing    -> False

-----------------------------------------------------------
--------- DISPLAY -----------------------------------------
-----------------------------------------------------------
view : (Int,Int) -> GameState -> Element
view (w,h) {snake, snake2, boardSize, apples} =
  collage w h
    <| renderScore snake.score :: renderScore snake2.score :: renderBoard boardSize :: List.append (List.append (renderApples apples) (renderSquares snake.parts snakeColor)) ((renderSquares snake2.parts snake2Color))

renderScore : Int -> Form
renderScore score =
  move (0,(boardSize / 2) + 10)
    <| toForm (show score)
  

renderBoard : Float -> Form
renderBoard size =
  filled boardColor
    <| square size

renderApples : List Apple -> List Form
renderApples apples =
  List.map (\a -> placeSegment (ownerColor a) a.square) apples

ownerColor : Apple -> Color.Color
ownerColor apple =
  case apple.owner of
    Just snake -> snake.color
    Nothing -> appleColor

renderSquares : List Square -> Color.Color -> List Form
renderSquares squares color =
  List.map (placeSegment color) squares


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
ensureListTail list =
  let
    maybeTail = List.tail list
  in
    case maybeTail of
      Just tail -> tail
      Nothing -> []
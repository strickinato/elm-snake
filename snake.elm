import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Array
import Random
import String
-----------------------------------------------------------
--------- CONFIG ------------------------------------------
-----------------------------------------------------------
boardSize        = 700
scale            = 10
frameSpeed       = 15
startingLength   = 10
startingApples   = 50
gameLength       = 60

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

port startTime : Int


type Input
  = Dir Direction
  | Dir2 Direction
  | Delta Float

type Chomp
  = Food
  | Tail
  | Border
  | Air

type State
  = Playing
  | Waiting

type alias Direction = { x : Int, y : Int }

type alias Square = (Int, Int)

type alias Snake =
  { direction:Direction
  , parts:List(Square)
  , chomp:Chomp
  , color:Color.Color
  , id:Int
  }

type alias Apple =
  { square:Square
  , seed:Random.Seed
  , owner:Maybe Snake
  }

type alias GameState =
  { snake:Snake
  , snake2:Snake
  , boardSize:Float
  , apples:List Apple
  , time: Int
  , state: State
  }


defaultSnake : Int -> Color.Color -> Snake
defaultSnake dir color=
  { direction = { x = 0 , y = dir }
  , parts = List.map (\(x,y) -> (x * dir, y)) (defaultParts startingLength)
  , chomp = Air
  , color = color
  , id = dir
  }


defaultParts : Int -> List Square
defaultParts length =
  if length == 0
    then []
    else (length - 1 - startingLength, 0) :: defaultParts (length - 1)


defaultApples : Int -> List Apple
defaultApples length =
  if length == 0
    then []
    else placeApple (appleConstructor (startTime * length)) :: defaultApples (length - 1)


appleConstructor : Int -> Apple
appleConstructor x =
  { square = (x, x)
  , seed = Random.initialSeed x
  , owner = Nothing 
  }


placeApple : Apple -> Apple
placeApple apple =
  let
    (x, seed')  = Random.generate (Random.int -range range) apple.seed
    (y, seed'') = Random.generate (Random.int -range range) seed'
  in
    { square = (x,y), seed = seed'', owner = apple.owner}


defaultGame : GameState
defaultGame =
  { snake = defaultSnake -1 snakeColor
  , snake2 = defaultSnake 1 snake2Color
  , boardSize = boardSize
  , apples = defaultApples startingApples
  , time = gameLength * frameSpeed
  , state = Playing
  }


range : Int
range = (boardSize // (scale * 2)) - 1


outsideBorder : Square -> Bool
outsideBorder (x, y) =
  (abs x >= abs range) || (abs y >= abs range)


-----------------------------------------------------------
--------- UPDATE ------------------------------------------
-----------------------------------------------------------
stepGame : Input -> GameState -> GameState
stepGame input ({snake, snake2, time} as gameState) =
  if time > 0 then
    case input of
      Dir direction ->
        { gameState | snake <- updateSnakeDirection direction snake
                    , time  <- (\t -> t - 1) time
        }

      Dir2 direction ->
        { gameState | snake2 <- updateSnakeDirection direction snake2
                    , time  <- (\t -> t - 1) time
        }

      Delta _ ->
        { gameState | snake  <- updateSnake snake gameState
                    , snake2 <- updateSnake snake2 gameState
                    , apples <- checkForSteals gameState
                    , time  <- (\t -> t - 1) time
        }
  else
    gameState


updateSnake : Snake -> GameState -> Snake
updateSnake snake gameState =
  let
    snakeState = checkSnakeStatus snake gameState
  in 
    case snakeState of
      Air ->
        movingSnake snake
      Food ->
        eatingSnake snake
      Tail ->
        snake
      Border ->
        snake


updateSnakeDirection : Direction -> Snake -> Snake
updateSnakeDirection direction snake =
  let
    current = snake.direction
  in
    if | direction == {x = 0, y = 0} -> snake
       | direction == {x = -current.x, y = -current.y} -> snake --Don't turn on yourself, snake!
       | otherwise -> { snake | direction <- direction }


checkSnakeStatus : Snake -> GameState -> Chomp
checkSnakeStatus snake {apples} =
  let
    snakeTail      = ensureListTail snake.parts
    appleLocations = (List.map (\a -> a.square) apples)
  in
    if | List.any (collision snake) appleLocations -> Food
       | List.any (collision snake) snakeTail -> Tail
       | List.any outsideBorder snake.parts -> Border
       | otherwise -> Air


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
    head :: tail -> step direction head :: head :: if eating then tail else (chopped tail)


step : Direction -> Square -> Square
step direction square =
  ( fst square + direction.x
  , snd square + direction.y
  )


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


collision : Snake -> Square -> Bool
collision snake square =
  let 
    mouth = List.head snake.parts
  in
    case mouth of
      Just chomp -> chomp == square
      Nothing    -> False

scoreFor : Snake -> List Apple -> Int
scoreFor snake apples =
  List.length (List.filterMap (\n -> isSnake n.owner snake ) apples)


isSnake : Maybe Snake -> Snake -> Maybe Snake
isSnake owner snake =
  case owner of
    Just owner -> if owner.id == snake.id then Just owner else Nothing
    Nothing    -> Nothing

-----------------------------------------------------------
--------- DISPLAY -----------------------------------------
-----------------------------------------------------------
view : (Int,Int) -> GameState -> Element
view (w,h) ({snake, snake2, boardSize, apples, time} as gameState) =
  collage w h
    <| List.append [toForm (renderScoreboard (w,h) gameState)]
    <| List.append [renderBoard boardSize]
    <| List.append (renderApples apples)
    <| List.append (renderSquares snake.parts snakeColor)
    <|             (renderSquares snake2.parts snake2Color)



renderScoreboard : (Int,Int) -> GameState -> Element
renderScoreboard (w,h) ({snake, snake2, boardSize, apples, time} as gameState) =
  collage w h
    <| List.append (renderPanels snake snake2 apples (w,h))
    <| List.append (renderScores gameState) [renderTime time]


renderPanels : Snake -> Snake -> List Apple -> (Int, Int) -> List Form
renderPanels snake snake2 apples (w,h) =
  let
    fullWidth = toFloat w
    fullHeight = toFloat h

    snakePoints  = scoreFor snake apples
    snake2Points = scoreFor snake2 apples
    snakeWidth   = (fullWidth * ( (toFloat snakePoints) / toFloat startingApples ))
    snake2Width  = (fullWidth * ( (toFloat snake2Points) / toFloat startingApples ))
  in
    [ rect (fullWidth) (fullHeight)
        |> filled appleColor
    , rect snakeWidth (fullHeight)
        |> filled snakeColor
        |> move (-1 * ((fullWidth / 2) - (snakeWidth / 2)), 0)
    , rect snake2Width (fullHeight)
        |> filled snake2Color
        |> move ((fullWidth / 2) - (snake2Width / 2), 0)
    ]


renderTime : Int -> Form
renderTime time =
  timeFormat (time // frameSpeed)
  |> show
  |> toForm
  |> move (0, (boardSize / 2) + 10)


renderScores : GameState -> List Form
renderScores {snake, snake2, boardSize, apples, time} =
  [renderScore (scoreFor snake apples) (-1 * ((boardSize / 2) + 100)), renderScore (scoreFor snake2 apples) ((boardSize / 2) + 100)]


renderScore : Int -> Float -> Form
renderScore score offset =
  toString score
  |> Text.fromString
  |> Text.height 36
  |> Text.monospace
  |> text
  |> move (offset, 0)
  

renderBoard : Float -> Form
renderBoard size =
  square size |> filled boardColor


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
  square (scaled 1)
  |> filled color
  |> move (scaled x, scaled y)
    

-----------------------------------------------------------
--------- UTILITY -----------------------------------------
-----------------------------------------------------------
chopped : List a -> List a
chopped list =
  case list of
    head :: []   -> []
    head :: tail -> head :: chopped tail


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


timeFormat : Int -> String
timeFormat time =
  let
    minutes = time // 60
    seconds = time % 60
  in
    if | seconds >= 10 -> String.concat [toString minutes, ":", toString seconds]
       | otherwise     -> String.concat [toString minutes, ":0", toString seconds]
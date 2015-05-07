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
  Signal.map inSeconds (fps 1)

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

(gameWidth, gameHeight) = (800,800)

type alias Board = Array(Array(Tile))

type alias Direction = { x : Int, y : Int }

type alias Square = (Int, Int)

type alias Tile = { state:Bool, coords:Square}

type alias Snake =
  {direction:Direction, parts:List(Square)}

type alias GameState =
  {snake:Snake, board:Board}

defaultSnake =  { direction = { x = 0, y = 0 }
                , parts = [(0,0),(1,0),(2,0)]
                }

createBoard : Int -> Array(Array(Tile))
createBoard x =
  let board = repeat x ( repeat x {state = False, coords = (0, 0)} )

  in indexedMap setRow board

setRow : Int -> Array(Tile) -> Array(Tile)
setRow index row = indexedMap (setTile False index) row

setTile : Bool -> Int -> Int -> Tile -> Tile
setTile state y x tile = { state = state
                         , coords = (x, y)
                         }

defaultGame : GameState
defaultGame =
    { snake    = defaultSnake
    , board    = createBoard 20
    }

-----------------------------------------------------------
--------- UPDATE ------------------------------------------
-----------------------------------------------------------


stepGame : Input -> GameState -> GameState
stepGame {timeDelta, direction} ({snake, board} as gameState) =
  let
    newSnake = updateSnake direction snake
    newBoard = updateBoard gameState
  in
      { gameState |
          board <- newBoard,
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
    {snake |
      direction <- newDirection
      , parts <- newParts
      }

updateParts : Direction -> List(Square) -> List(Square)
updateParts direction squares =
  case squares of
    []               -> []
    square :: squares -> move direction square :: updateParts direction squares

move : Direction -> Square -> Square
move direction square = ( fst square + direction.x
                        , snd square + direction.y
                        )

updateBoard : GameState -> Board
updateBoard ({snake, board} as gameState) =
  board

-----------------------------------------------------------
--------- DISPLAY -----------------------------------------
-----------------------------------------------------------
view : (Int,Int) -> GameState -> Element
view (w,h) {board, snake} =
  flow down (show snake :: (displayBoard board))

displayBoard : Board -> List(Element)
displayBoard board =
  (toList (map displayRow board))

displayRow: Array(Tile) -> Element
displayRow row =
  flow right  (toList (map displayTile row))

displayTile: Tile -> Element
displayTile tile =
  case tile.state of
    True -> collage 20 20 ([filled (Color.rgb 0 0 200) (square 20)])
    False -> collage 20 20 ([filled (Color.rgb 0 200 200) (square 20)])
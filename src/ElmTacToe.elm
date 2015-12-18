module ElmTacToe where

import Html exposing (..)
import StartApp.Simple as StartApp
import Signal exposing (Address)

import Board exposing (..)
import Symbol exposing (..)

type Action
  = NoOp
  | Take (Int, Int)

type alias Model =
  { nextPlayer: Symbol,
    board: Board
  }

--------------------------------------- MODEL

initialModel : Model
initialModel =
  { nextPlayer = Circle,
    board = Board.new
  }

-------------------------------------- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Take (x, y) ->
      model

---------------------------------------- VIEW

view : Address Action -> Model -> Html
view address model =
  Board.render model.board

---------------------------------------- MAIN

main : Signal Html
main =
  StartApp.start
            { model = initialModel,
              view = view,
              update = update
            }

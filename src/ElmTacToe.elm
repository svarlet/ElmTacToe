module ElmTacToe where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (id)
import StartApp.Simple as StartApp
import Signal exposing (Address)

import Board exposing (..)
import Symbol exposing (Symbol(..))

type Action
  = NoOp
  | Take (Int, Int)

type alias Model =
  { currentPlayer: Symbol
  , board: Board
  , state: Status
  }

--------------------------------------- MODEL

initialModel : Model
initialModel =
  { currentPlayer = Circle
  , board = Board.new
  , state = Playable
  }

oppositePlayer : Symbol -> Symbol
oppositePlayer symbol =
  case symbol of
    Circle ->
      Cross

    Cross ->
      Circle

-------------------------------------- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Take (x, y) ->
      let
        updatedBoard = Board.take x y model.currentPlayer model.board
      in
        { model |
            board = updatedBoard,
            currentPlayer = oppositePlayer model.currentPlayer,
            state = boardStatus updatedBoard
        }

---------------------------------------- VIEW

view : Address Action -> Model -> Html
view address model =
  let
    enabledClickHandler = \x y -> onClick address (Take (x,y))
    disabledClickHandler = \_ _ -> onClick address (NoOp)
  in
    case model.state of
      Win symbol ->
        div
          [ ]
          [ Board.render disabledClickHandler model.board
          , div [ id "game-over" ] [ text <| (toString symbol) ++ " wins" ]
          ]

      Lost ->
        div
          [ ]
          [ Board.render disabledClickHandler model.board
          , div [ id "game-over" ] [ text "Game over, nobody wins!" ]
          ]

      Playable ->
        div
          [ ]
          [ Board.render enabledClickHandler model.board ]


---------------------------------------- MAIN

main : Signal Html
main =
  StartApp.start
    { model = initialModel
    , view = view
    , update = update
    }

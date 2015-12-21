module Cell (Cell, render) where

import Symbol exposing (Symbol(..))
import Html exposing (..)
import Signal exposing (Address)
import Html.Events exposing (..)

type alias Cell
  = Maybe Symbol

render : Cell -> Attribute -> Html
render cell clickHandler =
  case cell of
    Nothing ->
      td
        [ clickHandler ]
        [ ]

    Just Circle ->
      td
        [ clickHandler ]
        [ text "O" ]

    Just Cross ->
      td
        [ clickHandler ]
        [ text "X" ]



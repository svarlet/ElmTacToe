module Cell where

import Symbol exposing (..)
import Html exposing (..)

type alias Cell
  = Maybe Symbol

render : Cell -> Html
render cell =
  case cell of
    Nothing ->
      td [] [text "_"]

    Just Circle ->
      td [] [text "O"]

    Just Cross ->
      td [] [text "X"]



module Board where

import Array exposing (Array)
import Matrix exposing (Matrix, map, repeat, getRow)
import Html exposing (..)

import Symbol exposing (..)
import Cell exposing (..)

type alias Cell
  = Maybe Symbol

type alias Board
  = Matrix Cell

new : Board
new =
  repeat 9 9 Nothing

take : (Int, Int) -> Symbol -> Board -> Board
take (x,y) symbol board =
  board

tableRows : Matrix Html -> List Html
tableRows board =
  [0..8]
  |> List.map (\i -> getRow i board)
  |> List.map (\arr -> case arr of
                         Nothing ->
                           tr [] []

                         Just arr ->
                           tr [] (Array.toList arr)
              )

render : Board -> Html
render board =
  board
    |> map Cell.render
    |> tableRows
    |> table []

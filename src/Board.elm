module Board (Board,
              Status(..),
              new,
              take,
              boardStatus,
              render
             ) where

import Array exposing (Array)
import Matrix exposing (Matrix, repeat, getRow, getColumn)
import Html exposing (Html, Attribute, table, tr)
import Html.Events exposing (onClick)
import List exposing ((::), head, tail)
import Signal exposing (Address)

import Symbol exposing (Symbol(..))
import Cell exposing (Cell, render)

type alias Board =
  Matrix Cell

type Status
  = Win Symbol
  | Lost
  | Playable

new : Board
new =
  repeat 3 3 Nothing

take : Int -> Int -> Symbol -> Board -> Board
take rowIndex colIndex symbol board =
  Matrix.set rowIndex colIndex (Just symbol) board

lineStatus : List Cell -> Status
lineStatus cells =
  if onlyOf (Just Cross) cells then
    Win Cross
  else if onlyOf (Just Circle) cells then
    Win Circle
  else if 2 <= List.length (List.filter ((==) Nothing) cells) then
    Playable
  else
    Lost

onlyOf : Cell -> List Cell -> Bool
onlyOf cellValue cells =
  List.all ((==) cellValue) cells

boardStatus : Board -> Status
boardStatus board =
  [ getRow 0 board,
    getRow 1 board,
    getRow 2 board,
    getColumn 0 board,
    getColumn 1 board,
    getColumn 2 board,
    getDiagonalStartingAt 0 0 board,
    getDiagonalStartingAt 0 2 board
  ]
  |> List.filterMap identity
  |> List.map Array.toList
  |> boardStatusFromLines

getDiagonalStartingAt : Int -> Int -> Board -> Maybe (Array Cell)
getDiagonalStartingAt x y board =
  case (x,y) of
    (0,0) ->
      Just <| Array.initialize 3 (\i -> Maybe.withDefault Nothing <| Matrix.get i i board)

    (0, 2) ->
      Just <| Array.initialize 3 (\i -> Maybe.withDefault Nothing <| Matrix.get i (2 - i) board)

    (2, 0) ->
      Just <| Array.initialize 3 (\i -> Maybe.withDefault Nothing <| Matrix.get (2 - i) i board)

    (2, 2) ->
      Just <| Array.initialize 3 (\i -> Maybe.withDefault Nothing <| Matrix.get (2 - i) (2 - i) board)

    _ ->
      Nothing

boardStatusFromLines : List (List Cell) -> Status
boardStatusFromLines lines =
  let
    initialStats = { playable = 0, lost = 0, win = Playable }
    statsUpdater = \line stats ->
                     case lineStatus line of
                       Win symbol ->
                         { stats | win = Win symbol }

                       Playable ->
                         { stats | playable = stats.playable + 1 }

                       Lost ->
                         { stats | lost = stats.lost + 1 }
    stats = List.foldl statsUpdater initialStats lines
  in
    case stats.win of
      Win symbol ->
        Win symbol

      _ ->
        case stats.playable of
          0 ->
            Lost

          _ ->
            Playable

render : (Int -> Int -> Attribute) -> Board -> Html
render clickHandler board =
  board
    |> Matrix.indexedMap (cellRenderer clickHandler)
    |> rows
    |> List.map (tr [])
    |> table []

cellRenderer : (Int -> Int -> Attribute) -> Int -> Int -> Cell -> Html
cellRenderer clickHandler x y c =
  clickHandler x y
    |> Cell.render c

rows : Matrix Html -> List (List Html)
rows matrix =
  List.map (\i -> getRow i matrix) [0..2]
  |> List.filterMap identity
  |> List.map Array.toList

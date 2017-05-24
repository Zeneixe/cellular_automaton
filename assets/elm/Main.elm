module Main exposing (..)

import Array exposing (..)
import Maybe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

type CellValue
    = Empty
    | Full


type alias Grid = { rows : List CellRow }

type alias CellRow = { cells : List Cell }

type alias Cell = { value : CellValue }

type alias Model = { grid : Grid }

automateCell : Int -> CellRow -> Cell
automateCell i prev =
    let
        prevArray = prev.cells |> Array.fromList
        temp = withDefault ( Cell Empty ) ( Array.get i prevArray )
    in
        Cell Full

automateRow : Int -> CellRow -> CellRow
automateRow n prev =
    let
        rowSize = 1 + (2 * n)
        range = List.range 1 rowSize
        row = CellRow ( range |> List.map ( \i -> automateCell i prev ) )
    in
        row

initialRow : CellRow
initialRow =
    CellRow [ Cell Full ]

initialGrid : Grid
initialGrid =
    let
        row0 = initialRow
        row1 = automateRow 1 row0
        row2 = automateRow 2 row1
        row3 = automateRow 3 row2
    in
        Grid [ row0, row1, row2, row3 ]

model : Model
model =
    Model initialGrid

cellClass : Cell -> String
cellClass c =
    if c.value == Full then
      "cell cell--empty"
    else
      "cell cell--full"

renderGrid : Grid -> Html Msg
renderGrid g =
    div [ class "cell-grid"]
        ( List.map (\r -> renderCellRow r) g.rows )

renderCellRow : CellRow -> Html Msg
renderCellRow r =
    div [ class "cell-row" ]
        ( List.map (\c -> renderCell c) r.cells )

renderCell : Cell -> Html Msg
renderCell c =
    div [ class (cellClass c) ] [ ]

view : Model -> Html Msg
view model =
    renderGrid model.grid

type Msg
    = None

update : Msg -> Model -> Model
update msg model =
    model

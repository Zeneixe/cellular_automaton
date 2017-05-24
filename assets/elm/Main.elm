module Main exposing (..)

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

type alias CellRow = { cells : List Cell }

type alias Cell = { value : CellValue }

type alias Model = { value : Int }

model : Model
model =
    { value = 0 }

cellClass : Cell -> String
cellClass c =
    if c.value == Full then
      "cell cell--empty"
    else
      "cell cell--full"

renderCellRow : Html Msg
renderCellRow =
    div [ class "cell-row" ]
        [ renderCell { value = Empty }
        , renderCell { value = Full }
        ]

renderCell : Cell -> Html Msg
renderCell c =
    div [ class (cellClass c) ] [ ]

view : Model -> Html Msg
view model =
    renderCellRow

type Msg
    = None

update : Msg -> Model -> Model
update msg model =
    model

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


type alias Grid = { rows : List CellRow }

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
    renderGrid
        ( Grid
            [ CellRow
                [ Cell Full
                ]
            , CellRow
                [ Cell Empty
                , Cell Full
                , Cell Empty
                ]
            ]
        )

type Msg
    = None

update : Msg -> Model -> Model
update msg model =
    model

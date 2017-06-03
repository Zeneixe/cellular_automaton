module Main exposing (..)

import Array exposing (..)
import Maybe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

main : Program Never Model Msg
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

type alias Rule =
    { prevLeft : CellValue
    , prevMiddle : CellValue
    , prevRight : CellValue
    , value : CellValue
    }

rules : List Rule
rules =
    [ Rule Full Full Full Empty
    , Rule Full Full Empty Empty
    , Rule Full Empty Full Empty
    , Rule Full Empty Empty Full
    , Rule Empty Full Full Full
    , Rule Empty Full Empty Full
    , Rule Empty Empty Full Full
    , Rule Empty Empty Empty Empty
    ]

checkRulesForCell : List Rule -> CellValue -> CellValue -> CellValue -> CellValue
checkRulesForCell rules prevLeft prevMiddle prevRight =
    let
        results = List.map (\rule ->
            if (rule.prevLeft == prevLeft) && (rule.prevMiddle == prevMiddle) && (rule.prevRight == prevRight) then
              rule.value
            else
              Empty
        ) rules
    in
        if (List.any (\c -> c == Full) results) then
          Full
        else
          Empty

automateCell : Int -> CellRow -> Cell
automateCell i prev =
    let
        prevArray = prev.cells |> Array.fromList
        prevLeft = withDefault ( Cell Empty ) ( Array.get (i - 2) prevArray )
        prevMiddle = withDefault ( Cell Empty ) ( Array.get (i - 1) prevArray )
        prevRight = withDefault ( Cell Empty ) ( Array.get i prevArray )
        value = checkRulesForCell rules prevLeft.value prevMiddle.value prevRight.value
    in
        Cell value

automateRow : Int -> CellRow -> CellRow
automateRow n prev =
    let
        rowSize = 1 + (2 * n)
        range = List.range 0 (rowSize - 1)
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
        row4 = automateRow 4 row3
        row5 = automateRow 5 row4
        row6 = automateRow 6 row5
        row7 = automateRow 7 row6

        row8 = automateRow 8 row7
        row9 = automateRow 9 row8
        row10 = automateRow 10 row9
        row11 = automateRow 11 row10
        row12 = automateRow 12 row11
        row13 = automateRow 13 row12
        row14 = automateRow 14 row13
        row15 = automateRow 15 row14
        row16 = automateRow 16 row15
        row17 = automateRow 17 row16
        row18 = automateRow 18 row17
        row19 = automateRow 19 row18
    in
        Grid [ row0, row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11, row12, row13, row14, row15, row16, row17, row18, row19 ]

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

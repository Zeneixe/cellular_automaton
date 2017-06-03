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

automateGrid : number -> List CellRow -> number1 -> CellRow -> List CellRow
automateGrid maxRows grid index prev =
    let
        newRow = automateRow index prev
        newGrid = grid ++ [newRow]
        newIndex = index + 1
    in
      if (index < maxRows) then
          automateGrid maxRows newGrid newIndex newRow
      else
          grid

model : Model
model =
    let
        initialGrid = Grid (automateGrid 125 [initialRow] 1 initialRow)
    in
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

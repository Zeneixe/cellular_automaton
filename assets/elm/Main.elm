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

type CellSize
    = Small
    | Large

type alias Grid = { rows : List CellRow }

type alias CellRow = { cells : List Cell }

type alias Cell = { value : CellValue }

type alias Model = { grid : Grid, rules : Ruleset }

type RowRule = RowRule CellValue CellValue CellValue

type alias Ruleset =
    { rule1 : CellValue
    , rule2 : CellValue
    , rule4 : CellValue
    , rule8 : CellValue
    , rule16 : CellValue
    , rule32 : CellValue
    , rule64 : CellValue
    , rule128 : CellValue
    }

initRules : Ruleset
initRules =
    Ruleset Empty Full Full Full Full Empty Empty Empty

checkRulesForCell : CellValue -> CellValue -> CellValue -> CellValue
checkRulesForCell prevLeft prevMiddle prevRight =
    let
        rule = RowRule prevLeft prevMiddle prevRight
    in
        case rule of
            RowRule Empty Empty Empty -> Empty
            RowRule Empty Empty Full -> Full
            RowRule Empty Full Empty -> Full
            RowRule Empty Full Full -> Full
            RowRule Full Empty Empty -> Full
            RowRule Full Empty Full -> Empty
            RowRule Full Full Empty -> Empty
            RowRule Full Full Full -> Empty

automateCell : Int -> CellRow -> Cell
automateCell i prev =
    let
        prevArray = prev.cells |> Array.fromList
        prevLeft = withDefault ( Cell Empty ) ( Array.get (i - 2) prevArray )
        prevMiddle = withDefault ( Cell Empty ) ( Array.get (i - 1) prevArray )
        prevRight = withDefault ( Cell Empty ) ( Array.get i prevArray )
        value = checkRulesForCell prevLeft.value prevMiddle.value prevRight.value
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

automateGrid
    : number
    -> List CellRow
    -> number
    -> CellRow
    -> List CellRow
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
        rules = initRules
        initialGrid = Grid (automateGrid 125 [initialRow] 1 initialRow)
    in
        Model initialGrid rules


cellClass c s =
    classList
        [ ("cell", True)
        , ("cell--full", (c == Full))
        , ("cell--large", (s == Large))
        ]

viewGrid : Grid -> Html Msg
viewGrid g =
    div [ class "cell-grid"]
        ( List.map (\r -> viewCellRow r) g.rows )

viewCellRow : CellRow -> Html Msg
viewCellRow r =
    div [ class "cell-row" ]
        ( List.map (\c -> viewCell c.value Small) r.cells )

viewCell : CellValue -> CellSize -> Html Msg
viewCell c s =
    div [ (cellClass c s) ] [ ]

viewRules rules =
    div [ ]
        [ viewRule rules.rule1 1
        , viewRule rules.rule2 2
        , viewRule rules.rule4 2
        , viewRule rules.rule8 2
        , viewRule rules.rule16 2
        , viewRule rules.rule32 2
        , viewRule rules.rule64 2
        , viewRule rules.rule128 128
        ]

viewRule rule number =
    viewCell rule Large

view : Model -> Html Msg
view model =
    div []
        [ viewRules model.rules
        , viewGrid model.grid
        ]

type Msg
    = None

update : Msg -> Model -> Model
update msg model =
    model

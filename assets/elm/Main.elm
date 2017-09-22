module Main exposing (..)

import Array exposing (..)
import Maybe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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

cellClass : CellValue -> CellSize -> Attribute Msg
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


viewRules : Ruleset -> Html Msg
viewRules rules =
    div [ class "rules" ]
        [ viewRule rules.rule1 1
        , viewRule rules.rule2 2
        , viewRule rules.rule4 4
        , viewRule rules.rule8 8
        , viewRule rules.rule16 16
        , viewRule rules.rule32 32
        , viewRule rules.rule64 64
        , viewRule rules.rule128 128
        ]

viewRule : CellValue -> Int -> Html Msg
viewRule rule number =
    let
        ( ruleL, ruleM, ruleR ) = case number of
            1 -> ( Empty, Empty, Empty )
            2 -> ( Empty, Empty, Full )
            4 -> ( Empty, Full, Empty )
            8 -> ( Empty, Full, Full )
            16 -> ( Full, Empty, Empty )
            32 -> ( Full, Empty, Full )
            64 -> ( Full, Full, Empty )
            128 -> ( Full, Full, Full )
            _ -> ( Empty, Empty, Empty )
    in
        div [ class "rule", onClick ( SwitchRule number ) ]
            [ div [ class "rule__row" ]
                [ viewCell ruleL Large
                , viewCell ruleM Large
                , viewCell ruleR Large
                ]
            , div [ class "rule__row" ]
                [ viewCell rule Large
                ]
            ]

view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ viewRules model.rules
        , viewGrid model.grid
        ]

type Msg
    = SwitchRule Int


invert : CellValue -> CellValue
invert cell =
    if ( cell == Full ) then
        Empty
    else
        Full

update : Msg -> Model -> Model
update msg model =
    let
        oldRules = model.rules
        newRules = case msg of
            SwitchRule ruleName ->
                case ruleName of
                    1 -> { oldRules | rule1 = invert oldRules.rule1 }
                    2 -> { oldRules | rule2 = invert oldRules.rule2 }
                    4 -> { oldRules | rule4 = invert oldRules.rule4 }
                    8 -> { oldRules | rule8 = invert oldRules.rule8 }
                    16 -> { oldRules | rule16 = invert oldRules.rule16 }
                    32 -> { oldRules | rule32 = invert oldRules.rule32 }
                    64 -> { oldRules | rule64 = invert oldRules.rule64 }
                    128 -> { oldRules | rule128 = invert oldRules.rule128 }
                    _ -> oldRules
    in
        { model | rules = newRules }

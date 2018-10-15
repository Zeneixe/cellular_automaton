module Main exposing (main)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type CellSize
    = Small
    | Large


type alias Grid =
    { rows : List CellRow }


type CellValue
    = Empty
    | Full


type alias CellIndex =
    Int


type alias RowIndex =
    Int


type alias CellRow =
    { cells : List CellValue }


type alias Model =
    { grid : Grid, rules : Ruleset }


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


applyRuleset : CellValue -> CellValue -> CellValue -> Ruleset -> CellValue
applyRuleset prevLeft prevMiddle prevRight ruleset =
    let
        rule =
            ( prevLeft, prevMiddle, prevRight )
    in
    case rule of
        ( Empty, Empty, Empty ) ->
            ruleset.rule1

        ( Empty, Empty, Full ) ->
            ruleset.rule2

        ( Empty, Full, Empty ) ->
            ruleset.rule4

        ( Empty, Full, Full ) ->
            ruleset.rule8

        ( Full, Empty, Empty ) ->
            ruleset.rule16

        ( Full, Empty, Full ) ->
            ruleset.rule32

        ( Full, Full, Empty ) ->
            ruleset.rule64

        ( Full, Full, Full ) ->
            ruleset.rule128


generateCell : Ruleset -> CellIndex -> CellRow -> CellValue
generateCell ruleset i prev =
    let
        prevArray =
            prev.cells |> Array.fromList

        prevLeft =
            withDefault Empty (Array.get (i - 2) prevArray)

        prevMiddle =
            withDefault Empty (Array.get (i - 1) prevArray)

        prevRight =
            withDefault Empty (Array.get i prevArray)
    in
    ruleset |> applyRuleset prevLeft prevMiddle prevRight


generateRow : Ruleset -> RowIndex -> CellRow -> CellRow
generateRow ruleset n prev =
    let
        rowSize =
            1 + (2 * n)

        range =
            List.range 0 (rowSize - 1)

        row =
            CellRow (range |> List.map (\i -> generateCell ruleset i prev))
    in
    row


initialRow : CellRow
initialRow =
    CellRow [ Full ]


generateGrid :
    Ruleset
    -> RowIndex
    -> Grid
    -> Grid
generateGrid ruleset maxRows grid =
    let
        index =
            List.length grid.rows

        rowsArray =
            Array.fromList grid.rows

        prevRowIndex =
            index - 1

        lastRow =
            withDefault (CellRow []) (Array.get prevRowIndex rowsArray)

        newRow =
            generateRow ruleset index lastRow

        newGrid =
            Grid (grid.rows ++ [ newRow ])
    in
    if index < maxRows then
        generateGrid ruleset maxRows newGrid

    else
        grid


model : Model
model =
    let
        rules =
            initRules

        initialGrid =
            generateGrid rules 125 (Grid [ initialRow ])
    in
    Model initialGrid rules


cellClass : CellValue -> CellSize -> Attribute Msg
cellClass c s =
    classList
        [ ( "cell", True )
        , ( "cell--full", c == Full )
        , ( "cell--large", s == Large )
        ]


viewGrid : Grid -> Html Msg
viewGrid g =
    div [ class "cell-grid" ]
        (List.map (\r -> viewCellRow r) g.rows)


viewCellRow : CellRow -> Html Msg
viewCellRow r =
    div [ class "cell-row" ]
        (List.map (\c -> viewCell c Small) r.cells)


viewCell : CellValue -> CellSize -> Html Msg
viewCell c s =
    div [ cellClass c s ] []


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
        ( ruleL, ruleM, ruleR ) =
            case number of
                1 ->
                    ( Empty, Empty, Empty )

                2 ->
                    ( Empty, Empty, Full )

                4 ->
                    ( Empty, Full, Empty )

                8 ->
                    ( Empty, Full, Full )

                16 ->
                    ( Full, Empty, Empty )

                32 ->
                    ( Full, Empty, Full )

                64 ->
                    ( Full, Full, Empty )

                128 ->
                    ( Full, Full, Full )

                _ ->
                    ( Empty, Empty, Empty )
    in
    div [ class "rule", onClick (SwitchRule number) ]
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
    if cell == Full then
        Empty

    else
        Full


update : Msg -> Model -> Model
update msg model =
    let
        oldRules =
            model.rules

        newRules =
            case msg of
                SwitchRule ruleName ->
                    case ruleName of
                        1 ->
                            { oldRules | rule1 = invert oldRules.rule1 }

                        2 ->
                            { oldRules | rule2 = invert oldRules.rule2 }

                        4 ->
                            { oldRules | rule4 = invert oldRules.rule4 }

                        8 ->
                            { oldRules | rule8 = invert oldRules.rule8 }

                        16 ->
                            { oldRules | rule16 = invert oldRules.rule16 }

                        32 ->
                            { oldRules | rule32 = invert oldRules.rule32 }

                        64 ->
                            { oldRules | rule64 = invert oldRules.rule64 }

                        128 ->
                            { oldRules | rule128 = invert oldRules.rule128 }

                        _ ->
                            oldRules

        newGrid =
            generateGrid newRules 125 (Grid [ initialRow ])
    in
    { model | rules = newRules, grid = newGrid }

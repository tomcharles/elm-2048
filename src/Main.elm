module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import Keyboard
import Random
import Tuple exposing (first, second)
import Board exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board
    , nextId : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        board =
            Board.init
    in
        ( { board = board, nextId = 2 }, Random.generate SetUpBoard <| Random.pair (newRandomCell board) (newRandomCell board) )


type Direction
    = Up
    | Right
    | Down
    | Left


type Msg
    = SetUpBoard ( Cell, Cell )
    | PlaceNewCell Cell
    | Collapse Direction
    | InvalidKeyPress


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUpBoard ( cell1, cell2 ) ->
            let
                newBoard =
                    model.board
                        |> Board.replaceCell { cell1 | id = 0 }
                        |> Board.replaceCell { cell2 | id = 1 }
            in
                ( { model | board = newBoard }, Cmd.none )

        PlaceNewCell cell ->
            let
                newBoard =
                    model.board |> Board.replaceCell { cell | id = model.nextId }
            in
                ( { model | board = newBoard, nextId = model.nextId + 1 }, Cmd.none )

        Collapse direction ->
            let
                newModel =
                    case direction of
                        Up ->
                            { model | board = Board.collapseUp model.board }

                        Right ->
                            { model | board = Board.collapseRight model.board }

                        Down ->
                            { model | board = Board.collapseDown model.board }

                        Left ->
                            { model | board = Board.collapseLeft model.board }

                newCellCmd =
                    if newModel.board == model.board then
                        Cmd.none
                    else
                        Random.generate PlaceNewCell (newRandomCell newModel.board)
            in
                ( newModel, newCellCmd )

        InvalidKeyPress ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        upkeys =
            [ 38, 75, 87 ]

        rightkeys =
            [ 39, 76, 68 ]

        downkeys =
            [ 40, 74, 83 ]

        leftkeys =
            [ 37, 72, 65 ]
    in
        Keyboard.downs
            (\keyCode ->
                if List.member keyCode upkeys then
                    Collapse Up
                else if List.member keyCode rightkeys then
                    Collapse Right
                else if List.member keyCode downkeys then
                    Collapse Down
                else if List.member keyCode leftkeys then
                    Collapse Left
                else
                    InvalidKeyPress
            )


newRandomCell : Board -> Random.Generator Cell
newRandomCell board =
    let
        twoOrFour =
            Random.map <|
                \num ->
                    if num == 0 then
                        4
                    else
                        2

        availablePiece index =
            List.drop index (Board.availableCells board)
                |> List.take 1
                |> List.head
                |> Maybe.withDefault ( 9, 9 )
    in
        Random.map2
            (\( x, y ) value -> Cell 0 x y value)
            (Random.map availablePiece <|
                Random.int 0 <|
                    (List.length <| Board.availableCells board)
                        - 1
            )
            (twoOrFour <| Random.int 0 4)


view : Model -> Html Msg
view model =
    let
        rows =
            List.indexedMap boardRow (Board.rows model.board)
    in
        div [ class "Container" ]
            [ div [ class "Board" ] rows ]


boardRow : Int -> List ( Int, Int ) -> Html Msg
boardRow index row =
    let
        cells =
            List.indexedMap (boardCell index) row
    in
        div [ class "Row" ] cells


boardCell : Int -> Int -> ( Int, Int ) -> Html Msg
boardCell columnIndex rowIndex cell =
    -- Html.Keyed.node "div"
    --     [ class ("Cell Cell" ++ toString (second cell))
    --     , style
    --         [ ( "transform", "translate(" ++ toString (rowIndex * 150) ++ "px," ++ toString (columnIndex * 150) ++ "px)" )
    --         ]
    --     ]
    --     [ ( toString <| first cell, text (toString (second cell)) ) ]
    div
        [ class ("Cell Cell" ++ toString (second cell))
        , style
            [ ( "transform", "translate(" ++ toString (rowIndex * 150) ++ "px," ++ toString (columnIndex * 150) ++ "px)" )
            ]
        ]
        [ text (toString (second cell)) ]

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
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
    }


init : ( Model, Cmd Msg )
init =
    let
        rows =
            [ [ 0, 0, 0, 0 ]
            , [ 0, 2, 0, 0 ]
            , [ 0, 0, 4, 0 ]
            , [ 0, 0, 0, 0 ]
            ]
    in
        ( { board = Board.initFromRows rows }, Cmd.none )


type Direction
    = Up
    | Right
    | Down
    | Left


type Msg
    = Collapse Direction
    | InvalidKeyPress


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            in
                ( newModel, Cmd.none )

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


view : Model -> Html Msg
view model =
    let
        rows =
            List.map boardRow (Board.rows model.board)
    in
        div [ class "Container" ]
            [ div [ class "Board" ] rows ]


boardRow : List Int -> Html Msg
boardRow row =
    let
        cells =
            List.map boardCell row
    in
        div [ class "Row" ] cells


boardCell : Int -> Html Msg
boardCell value =
    div [ class ("Cell Cell" ++ toString value) ] [ text (toString value) ]

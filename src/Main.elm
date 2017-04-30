module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
import Random
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
        board =
            Board.init
    in
        ( { board = board }, Random.generate SetUpBoard <| Random.pair (newRandomCell board) (newRandomCell board) )


type Direction
    = Up
    | Right
    | Down
    | Left


type Msg
    = SetUpBoard ( ( Int, Int, Int ), ( Int, Int, Int ) )
    | PlaceNewCell ( Int, Int, Int )
    | Collapse Direction
    | InvalidKeyPress


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUpBoard ( cell1, cell2 ) ->
            let
                newBoard =
                    model.board
                        |> Board.replaceCell cell1
                        |> Board.replaceCell cell2
            in
                ( { model | board = newBoard }, Cmd.none )

        PlaceNewCell cell ->
            let
                newBoard =
                    model.board |> Board.replaceCell cell
            in
                ( { model | board = newBoard }, Cmd.none )

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
                ( newModel, Random.generate PlaceNewCell (newRandomCell newModel.board) )

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


newRandomCell : Board -> Random.Generator ( Int, Int, Int )
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
            (\( x, y ) value -> ( x, y, value ))
            (Random.map availablePiece <|
                Random.int 0 <|
                    (List.length <| Board.availableCells board)
                        - 1
            )
            (twoOrFour <| Random.int 0 3)


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

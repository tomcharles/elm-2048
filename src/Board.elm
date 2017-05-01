module Board
    exposing
        ( Board
        , Cell
        , init
        , initFromRows
        , collapseRow
        , rows
        , collapseRight
        , collapseLeft
        , collapseDown
        , collapseUp
        , replaceCell
        , availableCells
        )

import Tuple exposing (second, mapSecond)
import Matrix exposing (..)


type Board
    = Board (Matrix ( Int, Int ))


type alias Cell =
    { id : Int
    , row : Int
    , column : Int
    , value : Int
    }


init : Board
init =
    Board (Matrix.initWithDefault 4 4 ( 0, 0 ))


initFromRows : List (List ( Int, Int )) -> Board
initFromRows rows =
    Board (Matrix.initUnsafe rows)


initFromMatrix : Matrix ( Int, Int ) -> Board
initFromMatrix matrix =
    Board matrix


rows : Board -> List (List ( Int, Int ))
rows (Board matrix) =
    Matrix.unwrap matrix


collapseRow : List ( Int, Int ) -> List ( Int, Int )
collapseRow row =
    let
        double x =
            mapSecond ((*) 2) x

        collapse row =
            case row of
                a :: b :: c :: d :: [] ->
                    if second a == second b && second c == second d then
                        [ ( 0, 0 ), ( 0, 0 ), double a, double c ]
                    else if second a == second b then
                        [ ( 0, 0 ), double a, c, d ]
                    else if second c == second d then
                        [ ( 0, 0 ), a, b, double c ]
                    else if second b == second c then
                        [ ( 0, 0 ), a, double b, d ]
                    else
                        row

                _ ->
                    row
    in
        row
            |> List.partition ((==) 0 << Tuple.second)
            |> uncurry List.append
            |> collapse


collapseRight : Board -> Board
collapseRight board =
    board
        |> rows
        |> List.map collapseRow
        |> initFromRows


collapseLeft : Board -> Board
collapseLeft board =
    board
        |> rows
        |> List.map List.reverse
        |> List.map collapseRow
        |> List.map List.reverse
        |> initFromRows


collapseDown : Board -> Board
collapseDown (Board matrix) =
    matrix
        |> rotateLeft
        |> unwrap
        |> List.map collapseRow
        |> initUnsafe
        |> rotateRight
        |> initFromMatrix


collapseUp : Board -> Board
collapseUp (Board matrix) =
    matrix
        |> rotateRight
        |> unwrap
        |> List.map collapseRow
        |> initUnsafe
        |> rotateLeft
        |> initFromMatrix


replaceCell : Cell -> Board -> Board
replaceCell { id, row, column, value } board =
    initFromRows <|
        List.indexedMap
            (\ri r ->
                if ri == row then
                    List.indexedMap
                        (\ci c ->
                            if ci == column then
                                ( id, value )
                            else
                                c
                        )
                        r
                else
                    r
            )
            (rows board)


availableCells : Board -> List ( Int, Int )
availableCells board =
    List.filterMap identity <|
        List.concatMap identity <|
            List.indexedMap
                (\ri row ->
                    List.indexedMap
                        (\ci cell ->
                            if second cell == 0 then
                                Just ( ri, ci )
                            else
                                Nothing
                        )
                        row
                )
                (rows board)

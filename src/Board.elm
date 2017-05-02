module Board
    exposing
        ( Board
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

import Matrix exposing (..)


type Board
    = Board (Matrix Int)


init : Board
init =
    Board (Matrix.initWithDefault 4 4 0)


initFromRows : List (List Int) -> Board
initFromRows rows =
    Board (Matrix.initUnsafe rows)


initFromMatrix : Matrix Int -> Board
initFromMatrix matrix =
    Board matrix


rows : Board -> List (List Int)
rows (Board matrix) =
    Matrix.unwrap matrix


collapseRow : List Int -> List Int
collapseRow row =
    let
        collapse row =
            case row of
                a :: b :: c :: d :: [] ->
                    if a == b && c == d then
                        [ 0, 0, a * 2, c * 2 ]
                    else if c == d then
                        [ 0, a, b, c * 2 ]
                    else if b == c then
                        [ 0, a, b * 2, d ]
                    else if a == b then
                        [ 0, a * 2, c, d ]
                    else
                        row

                _ ->
                    row
    in
        row
            |> List.partition ((==) 0)
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


replaceCell : ( Int, Int, Int ) -> Board -> Board
replaceCell newCell board =
    case newCell of
        ( x, y, value ) ->
            initFromRows <|
                List.indexedMap
                    (\ri row ->
                        if ri == x then
                            List.indexedMap
                                (\ci column ->
                                    if ci == y then
                                        value
                                    else
                                        column
                                )
                                row
                        else
                            row
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
                            if cell == 0 then
                                Just ( ri, ci )
                            else
                                Nothing
                        )
                        row
                )
                (rows board)

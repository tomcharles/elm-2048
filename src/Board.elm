module Board
    exposing
        ( Board
        , collapseRow
        )

import Matrix exposing (..)


type Board
    = Board (Matrix Int)


collapseRow : List Int -> List Int
collapseRow row =
    let
        collapse row =
            case row of
                a :: b :: c :: d :: [] ->
                    if a == b && c == d then
                        [ 0, 0, a * 2, c * 2 ]
                    else if a == b then
                        [ 0, a * 2, c, d ]
                    else if c == d then
                        [ 0, a, b, c * 2 ]
                    else if b == c then
                        [ 0, a, b * 2, d ]
                    else
                        row

                _ ->
                    row
    in
        row
            |> List.partition ((==) 0)
            |> uncurry List.append
            |> collapse

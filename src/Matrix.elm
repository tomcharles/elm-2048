module Matrix
    exposing
        ( Matrix
        , init
        , initUnsafe
        , initWithDefault
        , unwrap
        , rotateRight
        , rotateLeft
        , row
        , column
        )


type Matrix a
    = Matrix (List (List a))


init : Int -> Int -> (Int -> Int -> a) -> Matrix a
init w h applyFn =
    initWithDefault w h Nothing
        |> unwrap
        |> List.indexedMap
            (\ri r ->
                List.indexedMap
                    (\ci _ ->
                        applyFn ri ci
                    )
                    r
            )
        |> Matrix


initWithDefault : Int -> Int -> a -> Matrix a
initWithDefault w h default =
    Matrix <|
        List.repeat w
            (List.repeat h default)


initUnsafe : List (List a) -> Matrix a
initUnsafe matrix =
    Matrix matrix


unwrap : Matrix a -> List (List a)
unwrap (Matrix matrix) =
    matrix


rotateRight : Matrix a -> Matrix a
rotateRight (Matrix matrix) =
    let
        unsafeHead l =
            case l of
                h :: t ->
                    h

                _ ->
                    Debug.crash "unsafeHead called with empty list"

        unsafeTail l =
            case l of
                h :: t ->
                    t

                _ ->
                    Debug.crash "unsafeTail called with empty list"

        transpose ll =
            case ll of
                (x :: xs) :: xss ->
                    let
                        heads =
                            List.map unsafeHead xss

                        tails =
                            List.map unsafeTail xss
                    in
                        (x :: heads) :: transpose (xs :: tails)

                _ ->
                    []
    in
        transpose matrix
            |> List.map List.reverse
            |> Matrix


rotateLeft : Matrix a -> Matrix a
rotateLeft matrix =
    matrix
        |> rotateRight
        |> rotateRight
        |> rotateRight


row : Int -> Matrix a -> List a
row i matrix =
    matrix
        |> unwrap
        |> List.drop i
        |> List.take 1
        |> List.head
        |> Maybe.withDefault []


column : Int -> Matrix a -> List a
column i matrix =
    matrix
        |> rotateRight
        |> row i

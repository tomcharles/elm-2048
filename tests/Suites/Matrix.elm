module Suites.Matrix exposing (all)

import ElmTest.Extra exposing (Test, describe, test)
import Expect
import Matrix exposing (..)


all : Test
all =
    describe "Matrix" <|
        let
            evensAsJust x y =
                if x % 2 == 0 then
                    Just 1
                else
                    Nothing
        in
            [ describe "init"
                [ test "it works" <|
                    \_ ->
                        let
                            actual =
                                init 5 5 evensAsJust

                            expectedUnwrapped =
                                [ [ Just 1, Just 1, Just 1, Just 1, Just 1 ]
                                , [ Nothing, Nothing, Nothing, Nothing, Nothing ]
                                , [ Just 1, Just 1, Just 1, Just 1, Just 1 ]
                                , [ Nothing, Nothing, Nothing, Nothing, Nothing ]
                                , [ Just 1, Just 1, Just 1, Just 1, Just 1 ]
                                ]
                        in
                            Expect.equal expectedUnwrapped (unwrap actual)
                ]
            , describe "initUnsafe"
                [ test "it assigns the given array to a matrix" <|
                    \_ ->
                        let
                            expectedUnwrapped =
                                [ [ 1, 2, 3, 4 ], [ 5, 6, 7, 8 ] ]

                            actual =
                                initUnsafe expectedUnwrapped
                        in
                            Expect.equal expectedUnwrapped (unwrap actual)
                ]
            , describe "unwrap"
                [ test "it returns the correct list of lists" <|
                    \_ ->
                        let
                            expected =
                                [ [ Nothing, Nothing, Nothing, Nothing, Nothing ]
                                , [ Nothing, Nothing, Nothing, Nothing, Nothing ]
                                , [ Nothing, Nothing, Nothing, Nothing, Nothing ]
                                , [ Nothing, Nothing, Nothing, Nothing, Nothing ]
                                , [ Nothing, Nothing, Nothing, Nothing, Nothing ]
                                ]

                            actual =
                                unwrap (initWithDefault 5 5 Nothing)
                        in
                            Expect.equal expected actual
                ]
            , describe "rotateRight"
                [ test "it rotates a 2x2" <|
                    \_ ->
                        let
                            expectedUnwrapped =
                                [ [ Nothing, Just 1 ]
                                , [ Nothing, Just 1 ]
                                ]

                            actual =
                                rotateRight (init 2 2 evensAsJust)
                        in
                            Expect.equal expectedUnwrapped (unwrap actual)
                , test "it rotates a 4x4" <|
                    \_ ->
                        let
                            expectedUnwrapped =
                                [ [ Nothing, Just 1, Nothing, Just 1 ]
                                , [ Nothing, Just 1, Nothing, Just 1 ]
                                , [ Nothing, Just 1, Nothing, Just 1 ]
                                , [ Nothing, Just 1, Nothing, Just 1 ]
                                ]

                            actual =
                                rotateRight (init 4 4 evensAsJust)
                        in
                            Expect.equal expectedUnwrapped (unwrap actual)
                , test "it rotates an uneven grid" <|
                    \_ ->
                        let
                            expectedUnwrapped =
                                [ [ Nothing, Just 1 ]
                                , [ Nothing, Just 1 ]
                                , [ Nothing, Just 1 ]
                                , [ Nothing, Just 1 ]
                                ]

                            actual =
                                rotateRight (init 2 4 evensAsJust)
                        in
                            Expect.equal expectedUnwrapped (unwrap actual)
                ]
            , describe "rotateLeft"
                [ test "it rotates a grid that has been rotated right back to where it came from" <|
                    \_ ->
                        let
                            initial =
                                init 4 4 evensAsJust

                            actual =
                                rotateLeft <| rotateRight initial
                        in
                            Expect.equal (unwrap initial) (unwrap actual)
                ]
            , describe "row"
                [ test "it returns the correct row" <|
                    \_ ->
                        let
                            xAsValue x _ =
                                x

                            expected =
                                [ 3, 3, 3, 3 ]

                            actual =
                                row 3 (init 4 4 xAsValue)
                        in
                            Expect.equal expected actual
                ]
            , describe "column"
                [ test "it returns the correct column" <|
                    \_ ->
                        let
                            yAsValue _ y =
                                y

                            expected =
                                [ 2, 2, 2, 2 ]

                            actual =
                                column 2 (init 4 4 yAsValue)
                        in
                            Expect.equal expected actual
                ]
            ]

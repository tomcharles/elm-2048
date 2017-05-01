module Suites.Main exposing (all)

import ElmTest.Extra exposing (Test, describe, test)
import Expect
import Main exposing (..)
import Board


all : Test
all =
    describe "Main"
        [ describe "Collapse"
            [ test "it collapses to the right" <|
                \_ ->
                    let
                        expected =
                            [ [ 0, 0, 0, 4 ]
                            , [ 0, 0, 0, 8 ]
                            , [ 0, 0, 0, 16 ]
                            , [ 0, 2, 4, 8 ]
                            ]

                        msg =
                            Collapse Right

                        initialRows =
                            [ [ 0, 2, 0, 2 ]
                            , [ 4, 0, 0, 4 ]
                            , [ 0, 0, 8, 8 ]
                            , [ 2, 4, 4, 4 ]
                            ]

                        ( actual, _ ) =
                            update msg (initialModel initialRows)
                    in
                        Expect.equal expected (Board.rows actual.board)
            , test "it collapses to the left" <|
                \_ ->
                    let
                        expected =
                            [ [ 4, 0, 0, 0 ]
                            , [ 8, 0, 0, 0 ]
                            , [ 16, 0, 0, 0 ]
                            , [ 8, 4, 2, 0 ]
                            ]

                        msg =
                            Collapse Left

                        initialRows =
                            [ [ 2, 0, 2, 0 ]
                            , [ 4, 0, 0, 4 ]
                            , [ 8, 8, 0, 0 ]
                            , [ 4, 4, 4, 2 ]
                            ]

                        ( actual, _ ) =
                            update msg (initialModel initialRows)
                    in
                        Expect.equal expected (Board.rows actual.board)
            , test "it collapses to the bottom" <|
                \_ ->
                    let
                        expected =
                            [ [ 0, 0, 0, 0 ]
                            , [ 2, 0, 0, 0 ]
                            , [ 4, 0, 0, 0 ]
                            , [ 8, 16, 8, 4 ]
                            ]

                        msg =
                            Collapse Down

                        initialRows =
                            [ [ 2, 0, 4, 0 ]
                            , [ 4, 0, 0, 2 ]
                            , [ 4, 8, 0, 0 ]
                            , [ 4, 8, 4, 2 ]
                            ]

                        ( actual, _ ) =
                            update msg (initialModel initialRows)
                    in
                        Expect.equal expected (Board.rows actual.board)
            , test "it collapses to the top" <|
                \_ ->
                    let
                        expected =
                            [ [ 8, 16, 8, 4 ]
                            , [ 4, 0, 0, 0 ]
                            , [ 2, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            ]

                        msg =
                            Collapse Up

                        initialRows =
                            [ [ 4, 8, 4, 2 ]
                            , [ 4, 8, 0, 0 ]
                            , [ 4, 0, 0, 2 ]
                            , [ 2, 0, 4, 0 ]
                            ]

                        ( actual, _ ) =
                            update msg (initialModel initialRows)
                    in
                        Expect.equal expected (Board.rows actual.board)
            , test "it spawns a new cell when the board is collapsable" <|
                \_ ->
                    let
                        msg =
                            Collapse Up

                        initialRows =
                            [ [ 4, 8, 4, 2 ]
                            , [ 4, 8, 0, 0 ]
                            , [ 4, 0, 0, 2 ]
                            , [ 2, 0, 4, 0 ]
                            ]

                        ( _, cmd ) =
                            update msg (initialModel initialRows)
                    in
                        Expect.notEqual Cmd.none cmd
            , test "it does not span a new cell when the board is not collapsable" <|
                \_ ->
                    let
                        msg =
                            Collapse Up

                        initialRows =
                            [ [ 4, 8, 4, 2 ]
                            , [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            ]

                        ( _, cmd ) =
                            update msg (initialModel initialRows)
                    in
                        Expect.equal Cmd.none cmd
            ]
        ]


initialModel : List (List Int) -> Model
initialModel rows =
    { board = Board.initFromRows rows, nextId = 0 }

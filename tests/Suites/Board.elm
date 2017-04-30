module Suites.Board exposing (all)

import ElmTest.Extra exposing (Test, describe, test)
import Expect
import Board exposing (..)


all : Test
all =
    describe "Board"
        [ describe "collapseRow"
            [ test "it collapses a list of zeros to a list of zeros" <|
                \_ ->
                    let
                        expected =
                            [ 0, 0, 0, 0 ]

                        actual =
                            collapseRow [ 0, 0, 0, 0 ]
                    in
                        Expect.equal expected actual
            , test "it collapses a row with one 2 at the end to the same thing" <|
                \_ ->
                    let
                        expected =
                            [ 0, 0, 0, 2 ]

                        actual =
                            collapseRow [ 0, 0, 0, 2 ]
                    in
                        Expect.equal expected actual
            , test "it moves a single uncollapsable number to the right" <|
                \_ ->
                    let
                        expected =
                            [ 0, 0, 0, 2 ]

                        actual =
                            collapseRow [ 0, 2, 0, 0 ]
                    in
                        Expect.equal expected actual
            , test "it moves multiple uncollapsable numbers to the right" <|
                \_ ->
                    let
                        expected =
                            [ 0, 0, 4, 16 ]

                        actual =
                            collapseRow [ 4, 0, 16, 0 ]
                    in
                        Expect.equal expected actual
            , test "it collapses two collapsable numbers and moves to the right" <|
                \_ ->
                    let
                        expected =
                            [ 0, 0, 0, 4 ]

                        actual =
                            collapseRow [ 0, 2, 0, 2 ]
                    in
                        Expect.equal expected actual
            , test "it collapse two collapsable numbers at the beginning and moves to the right" <|
                \_ ->
                    let
                        expected =
                            [ 0, 4, 8, 16 ]

                        actual =
                            collapseRow [ 2, 2, 8, 16 ]
                    in
                        Expect.equal expected actual
            , test "it collapses four separate collapsable numbers and moves to the right" <|
                \_ ->
                    let
                        expected =
                            [ 0, 0, 2, 8 ]

                        actual =
                            collapseRow [ 2, 0, 4, 4 ]
                    in
                        Expect.equal expected actual
            , test "it collapses two collapsable numbers in the middle and shifts right" <|
                \_ ->
                    let
                        expected =
                            [ 0, 0, 4, 8 ]

                        actual =
                            collapseRow [ 0, 2, 2, 8 ]
                    in
                        Expect.equal expected actual
            , test "it collapses two separate sets of collapsable numbers" <|
                \_ ->
                    let
                        expected =
                            [ 0, 0, 4, 8 ]

                        actual =
                            collapseRow [ 2, 2, 4, 4 ]
                    in
                        Expect.equal expected actual
            , test "it doesnt collapse when nothing matches" <|
                \_ ->
                    let
                        expected =
                            [ 2, 4, 8, 16 ]

                        actual =
                            collapseRow expected
                    in
                        Expect.equal expected actual
            ]
        , describe "rows"
            [ test "it returns board as a list of lists" <|
                \_ ->
                    let
                        expected =
                            [ [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            ]

                        actual =
                            rows Board.init
                    in
                        Expect.equal expected actual
            ]
        , describe "collapseRight"
            [ test "it collapses all rows to the right" <|
                \_ ->
                    let
                        expected =
                            [ [ 0, 0, 0, 2 ]
                            , [ 0, 0, 0, 4 ]
                            , [ 0, 0, 0, 8 ]
                            , [ 0, 0, 0, 16 ]
                            ]

                        beforeCollapse =
                            [ [ 0, 2, 0, 0 ]
                            , [ 0, 2, 2, 0 ]
                            , [ 4, 0, 0, 4 ]
                            , [ 8, 0, 8, 0 ]
                            ]

                        actual =
                            collapseRight (initFromRows beforeCollapse)
                    in
                        Expect.equal expected (rows actual)
            ]
        , describe "collapseLeft"
            [ test "it collapses all rows to the left" <|
                \_ ->
                    let
                        expected =
                            [ [ 2, 0, 0, 0 ]
                            , [ 4, 0, 0, 0 ]
                            , [ 8, 0, 0, 0 ]
                            , [ 16, 0, 0, 0 ]
                            ]

                        beforeCollapse =
                            [ [ 0, 2, 0, 0 ]
                            , [ 0, 2, 2, 0 ]
                            , [ 4, 0, 0, 4 ]
                            , [ 8, 0, 8, 0 ]
                            ]

                        actual =
                            collapseLeft (initFromRows beforeCollapse)
                    in
                        Expect.equal expected (rows actual)
            ]
        , describe "collapseDown"
            [ test "it collapses all the rows down" <|
                \_ ->
                    let
                        expected =
                            [ [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            , [ 2, 4, 8, 16 ]
                            ]

                        beforeCollapse =
                            [ [ 0, 0, 4, 8 ]
                            , [ 2, 2, 0, 0 ]
                            , [ 0, 2, 0, 8 ]
                            , [ 0, 0, 4, 0 ]
                            ]

                        actual =
                            collapseDown (initFromRows beforeCollapse)
                    in
                        Expect.equal expected (rows actual)
            , test "it does not flip when numbers are both already on bottom" <|
                \_ ->
                    let
                        expected =
                            [ [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            , [ 0, 2, 0, 0 ]
                            , [ 0, 4, 0, 0 ]
                            ]

                        actual =
                            collapseDown (initFromRows expected)
                    in
                        Expect.equal expected (rows actual)
            ]
        , describe "collapseUp"
            [ test "it collapses all the rows up" <|
                \_ ->
                    let
                        expected =
                            [ [ 2, 4, 8, 16 ]
                            , [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 0 ]
                            ]

                        beforeCollapse =
                            [ [ 0, 0, 4, 8 ]
                            , [ 2, 2, 0, 0 ]
                            , [ 0, 2, 0, 8 ]
                            , [ 0, 0, 4, 0 ]
                            ]

                        actual =
                            collapseUp (initFromRows beforeCollapse)
                    in
                        Expect.equal expected (rows actual)
            ]
        ]

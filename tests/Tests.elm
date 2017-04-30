module Tests exposing (..)

import ElmTest.Extra exposing (Test, describe)
import Suites.Matrix
import Suites.Board


all : Test
all =
    describe "elm-2048"
        [ Suites.Matrix.all
        , Suites.Board.all
        ]

module Tests exposing (..)

import ElmTest.Extra exposing (Test, describe)
import Suites.Main
import Suites.Matrix
import Suites.Board


all : Test
all =
    describe "elm-2048"
        [ Suites.Main.all
        , Suites.Matrix.all
        , Suites.Board.all
        ]

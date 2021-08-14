module Tests exposing (..)

import Expect
import Main exposing (DisplayTime, millisToDisplayTime)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "millisToDisplayTime with hours" <|
            \_ ->
                Expect.equal expected1 (millisToDisplayTime <| ((3 * 60 + 20) * 60 + 18) * 1000 + 729)
        , test "millisToDisplayTime with minus time" <|
            \_ ->
                Expect.equal expected2 (millisToDisplayTime -5000)
        ]


expected1 : DisplayTime
expected1 =
    { isMinus = False
    , hours = 3
    , minutes = 20
    , seconds = 18
    }


expected2 : DisplayTime
expected2 =
    { isMinus = True
    , hours = 0
    , minutes = 0
    , seconds = 5
    }

module Tests exposing (..)

import Expect
import Main exposing (..)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


millisToDisplayTimeTest : Test
millisToDisplayTimeTest =
    describe "Tests for millisToDisplayTime"
        [ test "millisToDisplayTime with hours" <|
            \_ ->
                millisToDisplayTime (((3 * 60 + 20) * 60 + 18) * 1000 + 729)
                    |> Expect.equal expected1
        , test "millisToDisplayTime with minus time" <|
            \_ ->
                millisToDisplayTime -5000
                    |> Expect.equal expected2
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


settingFromQueryTest : Test
settingFromQueryTest =
    describe "parseSettingFromQuery correctly transform initial parameters"
        [ test "with no parameters" <|
            \_ ->
                { fg = Nothing
                , bg = Nothing
                , init = Nothing
                , h = Nothing
                , p = Nothing
                }
                    |> parseSettingFromQuery
                    |> Expect.equal defaultSetting
        , test "with full parameters" <|
            \_ ->
                { fg = Just "#ffffff"
                , bg = Just "bb"
                , init = Just -5
                , h = Just "false"
                , p = Just "true"
                }
                    |> parseSettingFromQuery
                    |> Expect.equal
                        { fgColor = "#ffffff"
                        , bgColor = BlueBack
                        , initialTimeSeconds = -5
                        , showHour = False
                        , showProgress = True
                        }
        , test "with some inputs" <|
            \_ ->
                { fg = Just "#000000"
                , bg = Nothing
                , init = Just -30
                , h = Nothing
                , p = Nothing
                }
                    |> parseSettingFromQuery
                    |> Expect.equal { defaultSetting | fgColor = "#000000", initialTimeSeconds = -30 }
        , test "with some correct enums" <|
            \_ ->
                { fg = Nothing
                , bg = Just "tp"
                , init = Nothing
                , h = Just "true"
                , p = Just "true"
                }
                    |> parseSettingFromQuery
                    |> Expect.equal { defaultSetting | bgColor = Transparent, showHour = True, showProgress = True }
        , test "with invalud enums" <|
            \_ ->
                { fg = Nothing
                , bg = Just "aa"
                , init = Nothing
                , h = Just "test"
                , p = Just "hogehoge"
                }
                    |> parseSettingFromQuery
                    |> Expect.equal defaultSetting
        ]


selectBgColorTest : Test
selectBgColorTest =
    describe "selectBgColor correctly map String to Msg"
        [ test "check mapping" <|
            \_ ->
                [ "gb", "bb", "tp", "hoge" ]
                    |> List.map selectBgColor
                    |> Expect.equal [ SetBgColor GreenBack, SetBgColor BlueBack, SetBgColor Transparent, NoOp ]
        ]

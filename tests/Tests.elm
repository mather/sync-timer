module Tests exposing (..)

import Dict
import Expect
import Fuzz
import Main exposing (parseSettingFromQuery)
import Model exposing (BgColor(..), FgFont(..), defaultSetting, dictBgColor, dictFgFont)
import Msg exposing (Msg(..))
import Parser
import Test exposing (..)
import Url.Parser exposing (Parser)
import View exposing (DisplayTime, millisToDisplayTime, resetTimeValueParser, resetTimeValueToString, selectBgColor, selectFgFont)



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
                , ff = Nothing
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
                , ff = Just "lora"
                , init = Just -5
                , h = Just "false"
                , p = Just "true"
                }
                    |> parseSettingFromQuery
                    |> Expect.equal
                        { fgColor = "#ffffff"
                        , bgColor = BlueBack
                        , fgFont = Lora
                        , initialTimeSeconds = -5
                        , showHour = False
                        , showProgress = True
                        }
        , test "with some inputs" <|
            \_ ->
                { fg = Just "#000000"
                , bg = Nothing
                , ff = Just "lora"
                , init = Just -30
                , h = Nothing
                , p = Nothing
                }
                    |> parseSettingFromQuery
                    |> Expect.equal { defaultSetting | fgColor = "#000000", initialTimeSeconds = -30, fgFont = Lora }
        , test "with some correct enums" <|
            \_ ->
                { fg = Nothing
                , bg = Just "tp"
                , ff = Nothing
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
                , ff = Just "bb"
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
        , fuzz (Fuzz.oneOfValues <| Dict.keys <| dictBgColor) "any key can be mapped as SetBgColor" <|
            \k ->
                selectBgColor k
                    |> Expect.notEqual NoOp
        ]


selectFgFontTest : Test
selectFgFontTest =
    describe "selectFgFont correctly map String to Msg"
        [ test "check mapping" <|
            \_ ->
                [ "d-din-bold", "lora", "hoge" ]
                    |> List.map selectFgFont
                    |> Expect.equal [ SetFgFont DDinBold, SetFgFont Lora, NoOp ]
        , fuzz (Fuzz.oneOfValues <| Dict.keys <| dictFgFont) "any key can be mapped as SetFgFont" <|
            \k ->
                selectFgFont k
                    |> Expect.notEqual NoOp
        ]


resetTimeValueTest : Test
resetTimeValueTest =
    describe "ResetTimeValue can be converted to String and parsed from String correctly"
        [ test "convert to String" <|
            \_ ->
                { hours = 12, minutes = 34, seconds = 56 }
                    |> resetTimeValueToString
                    |> Expect.equal "12:34:56"
        , test "parse from String" <|
            \_ ->
                Parser.run resetTimeValueParser "01:23:45"
                    |> Expect.equal (Result.Ok { hours = 1, minutes = 23, seconds = 45 })
        , fuzz (Fuzz.intAtLeast 0) "no difference after conversion and parsing" <|
            \i ->
                { hours = i // 3600, minutes = i // 60 |> modBy 60, seconds = modBy 60 i }
                    |> resetTimeValueToString
                    |> Parser.run resetTimeValueParser
                    |> Result.map (\x -> x.hours * 3600 + x.minutes * 60 + x.seconds)
                    |> Expect.equal (Result.Ok i)
        ]

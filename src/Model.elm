module Model exposing (..)

import Dict
import Time


type alias Model =
    { timeMillis : Int
    , paused : Bool
    , current : Maybe Time.Posix
    , setting : Setting
    }


type alias Setting =
    { bgColor : BgColor
    , fgColor : String
    , fgFont : FgFont
    , initialTimeSeconds : Int
    , showHour : Bool
    , showProgress : Bool
    }


defaultSetting : Setting
defaultSetting =
    { fgColor = "#415462"
    , bgColor = GreenBack
    , fgFont = DDinBold
    , initialTimeSeconds = -10
    , showHour = True
    , showProgress = False
    }


type FgFont
    = DDinBold
    | Lora


encodeFgFont : FgFont -> String
encodeFgFont fgFont =
    case fgFont of
        DDinBold ->
            "d-din-bold"

        Lora ->
            "lora"


type BgColor
    = Transparent
    | GreenBack
    | BlueBack


dictFgFont : Dict.Dict String FgFont
dictFgFont =
    List.foldl (\bg -> Dict.insert (encodeFgFont bg) bg) Dict.empty [ DDinBold, Lora ]


decodeFgFont : String -> Maybe FgFont
decodeFgFont =
    flip Dict.get dictFgFont


encodeBgColor : BgColor -> String
encodeBgColor bg =
    case bg of
        GreenBack ->
            "gb"

        BlueBack ->
            "bb"

        Transparent ->
            "tp"


dictBgColor : Dict.Dict String BgColor
dictBgColor =
    List.foldl (\bg -> Dict.insert (encodeBgColor bg) bg) Dict.empty [ GreenBack, BlueBack, Transparent ]


decodeBgColor : String -> Maybe BgColor
decodeBgColor =
    flip Dict.get dictBgColor


encodeBoolean : Bool -> String
encodeBoolean b =
    if b then
        "true"

    else
        "false"


dictBoolean : Dict.Dict String Bool
dictBoolean =
    Dict.fromList [ ( "true", True ), ( "false", False ) ]


decodeBoolean : String -> Maybe Bool
decodeBoolean =
    flip Dict.get dictBoolean


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b

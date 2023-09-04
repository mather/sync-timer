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


dictEnum_ : (a -> String) -> List a -> Dict.Dict String a
dictEnum_ encoder elements =
    elements
        |> List.map (\e -> ( encoder e, e ))
        |> Dict.fromList


flip_ : (a -> b -> c) -> b -> a -> c
flip_ f b a =
    f a b


type FgFont
    = DDinBold
    | Lora
    | DSEG7ClassicBold


encodeFgFont : FgFont -> String
encodeFgFont fgFont =
    case fgFont of
        DDinBold ->
            "d-din-bold"

        Lora ->
            "lora"

        DSEG7ClassicBold ->
            "dseg7-classic-bold"


type BgColor
    = Transparent
    | GreenBack
    | BlueBack


dictFgFont : Dict.Dict String FgFont
dictFgFont =
    dictEnum_ encodeFgFont [ DDinBold, Lora, DSEG7ClassicBold ]


decodeFgFont : String -> Maybe FgFont
decodeFgFont =
    flip_ Dict.get dictFgFont


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
    dictEnum_ encodeBgColor [ GreenBack, BlueBack, Transparent ]


decodeBgColor : String -> Maybe BgColor
decodeBgColor =
    flip_ Dict.get dictBgColor


encodeBoolean : Bool -> String
encodeBoolean b =
    if b then
        "true"

    else
        "false"


dictBoolean : Dict.Dict String Bool
dictBoolean =
    dictEnum_ encodeBoolean [ True, False ]


decodeBoolean : String -> Maybe Bool
decodeBoolean =
    flip_ Dict.get dictBoolean

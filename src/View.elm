module View exposing (DisplayTime, ResetTimeValue, millisToDisplayTime, resetTimeValueParser, resetTimeValueToString, selectBgColor, selectFgFont, view)

import Html exposing (Attribute, Html, a, button, details, div, i, input, label, option, select, small, span, summary, text)
import Html.Attributes exposing (attribute, checked, class, for, href, id, selected, size, step, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import I18n
import Model exposing (BgColor(..), FgFont(..), Model, Setting, decodeBgColor, decodeFgFont, encodeBgColor, encodeFgFont)
import Msg exposing (Msg(..))
import Parser exposing ((|.), (|=))


type alias DisplayTime =
    { isMinus : Bool
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


millisToDisplayTime : Int -> DisplayTime
millisToDisplayTime t =
    let
        absSeconds =
            toFloat t / 1000 |> floor |> abs
    in
    { isMinus = t < 0
    , hours = absSeconds // 3600
    , minutes = absSeconds // 60 |> modBy 60
    , seconds = absSeconds |> modBy 60
    }


view : I18n.Label -> Model -> Html Msg
view label model =
    div [ class "container" ]
        [ viewTimer label model
        , viewTimerSettings label model.setting
        ]


viewTimer : I18n.Label -> Model -> Html Msg
viewTimer label model =
    div [ class "grid" ]
        [ viewTimerDigits model.timeMillis model.setting
        , viewTimerControls label model
        ]


viewTimerDigits : Int -> Setting -> Html Msg
viewTimerDigits millis setting =
    let
        displayTime =
            millisToDisplayTime millis

        displaySegments =
            if setting.showHour || displayTime.hours > 0 then
                [ displayTime.hours, displayTime.minutes, displayTime.seconds ]

            else
                [ displayTime.minutes, displayTime.seconds ]
    in
    div [ class "timer", fontClass setting.fgFont, timerBgColorClass setting.bgColor, style "color" setting.fgColor ]
        [ div [ class "digits" ] <|
            (List.concat <|
                [ [ span (styleTimerSign displayTime.isMinus) [ text "-" ] ]
                , List.foldr joinWithSegment [] <| List.map renderBig2Digits displaySegments
                ]
            )
                ++ viewProgressBar millis setting
        ]


fontClass : FgFont -> Html.Attribute Msg
fontClass =
    encodeFgFont >> class


progress : Int -> Int -> String
progress millis initialTimeSeconds =
    if millis < 0 then
        (String.fromFloat <| min 100.0 <| toFloat millis / (toFloat initialTimeSeconds * 10)) ++ "%"

    else
        "100%"


viewProgressBar : Int -> Setting -> List (Html Msg)
viewProgressBar millis setting =
    let
        visibility =
            style "visibility" <|
                if millis >= 0 then
                    "hidden"

                else
                    "visible"
    in
    if setting.showProgress then
        [ div
            [ class "progress"
            , style "width" <| progress millis setting.initialTimeSeconds
            , style "background-color" setting.fgColor
            , visibility
            ]
            []
        ]

    else
        []


joinWithSegment : List (Html Msg) -> List (Html Msg) -> List (Html Msg)
joinWithSegment a b =
    case b of
        [] ->
            a

        _ ->
            List.concat [ a, [ span styleTimerSep [ text ":" ] ], b ]


timerBgColorClass : BgColor -> Attribute Msg
timerBgColorClass bgColor =
    case bgColor of
        Transparent ->
            class "transparent"

        GreenBack ->
            class "greenback"

        BlueBack ->
            class "blueback"


padZero : Int -> Int -> String
padZero wt digits =
    String.fromInt digits |> String.padLeft wt '0'


styleTimerSep : List (Html.Attribute Msg)
styleTimerSep =
    [ class "sep" ]


styleTimerDigit : List (Html.Attribute Msg)
styleTimerDigit =
    [ class "digit" ]


styleTimerSign : Bool -> List (Html.Attribute Msg)
styleTimerSign isMinus =
    let
        visibility =
            style "visibility" <|
                if isMinus then
                    "visible"

                else
                    "hidden"
    in
    visibility :: styleTimerDigit


renderBig2Digits : Int -> List (Html Msg)
renderBig2Digits digits =
    padZero 2 digits
        |> String.split ""
        |> List.map (\d -> span styleTimerDigit [ text d ])


viewTimerControls : I18n.Label -> Model -> Html Msg
viewTimerControls label model =
    div [ class "controls" ]
        [ div [] [ startPauseButton label model.paused ]
        , div [ class "ff-buttons" ]
            [ rewindButton label
            , fastForwardButton label
            ]
        , div [] [ resetForm label model.setting.initialTimeSeconds ]
        ]


startPauseButton : I18n.Label -> Bool -> Html Msg
startPauseButton label paused =
    if paused then
        button
            [ onClick Start ]
            [ i [ class "fas", class "fa-play", class "button-icon" ] []
            , text label.start
            ]

    else
        button
            [ onClick Pause, class "secondary" ]
            [ i [ class "fas", class "fa-pause", class "button-icon" ] []
            , text label.pause
            ]


rewindButton : I18n.Label -> Html Msg
rewindButton label =
    button [ class "outline", onClick <| RewindSec 1 ]
        [ i [ class "fas", class "fa-backward", class "button-icon" ] []
        , text label.backward
        ]


fastForwardButton : I18n.Label -> Html Msg
fastForwardButton label =
    button [ class "outline", onClick <| FastForwardSec 1 ]
        [ text label.forward
        , i [ class "fas", class "fa-forward", class "button-icon" ] []
        ]


type alias ResetTime =
    { isMinus : Bool
    , timeValue : ResetTimeValue
    }


type alias ResetTimeValue =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


zeroPadIntParser : Parser.Parser Int
zeroPadIntParser =
    Parser.oneOf
        [ Parser.succeed identity |. Parser.chompIf ((==) '0') |= Parser.int
        , Parser.int
        ]


resetTimeValueParser : Parser.Parser ResetTimeValue
resetTimeValueParser =
    Parser.succeed ResetTimeValue
        |= zeroPadIntParser
        |. Parser.symbol ":"
        |= zeroPadIntParser
        |. Parser.symbol ":"
        |= zeroPadIntParser


resetTimeValueToString : ResetTimeValue -> String
resetTimeValueToString value =
    (String.padLeft 2 '0' <| String.fromInt value.hours)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt value.minutes)
        ++ ":"
        ++ (String.padLeft 2 '0' <| String.fromInt value.seconds)


initialTimeToResetTime : Int -> ResetTime
initialTimeToResetTime initialTimeSeconds =
    let
        absSeconds =
            abs initialTimeSeconds

        hours =
            absSeconds // 3600

        minutes =
            absSeconds // 60 |> modBy 60

        seconds =
            absSeconds |> modBy 60
    in
    { isMinus = initialTimeSeconds < 0
    , timeValue =
        { hours = hours
        , minutes = minutes
        , seconds = seconds
        }
    }


negateIf : Bool -> Int -> Int
negateIf isMinus value =
    if isMinus then
        negate value

    else
        value


updateResetTimeMinus : Int -> Bool -> Msg
updateResetTimeMinus initialTimeSeconds isMinus =
    abs initialTimeSeconds
        |> negateIf isMinus
        |> UpdateResetTime


updateResetTimeValue : Bool -> String -> Msg
updateResetTimeValue isMinus timeValue =
    Parser.run resetTimeValueParser timeValue
        |> Result.toMaybe
        |> Maybe.map (\t -> t.hours * 3600 + t.minutes * 60 + t.seconds)
        |> Maybe.map (negateIf isMinus)
        |> Maybe.map UpdateResetTime
        |> Maybe.withDefault NoOp


resetForm : I18n.Label -> Int -> Html Msg
resetForm i18n initialTimeSeconds =
    let
        resetTime =
            initialTimeToResetTime initialTimeSeconds
    in
    div [ class "reset-form" ]
        [ label [ for "reset-is-minus", class "reset-form-minus" ]
            [ input [ id "reset-is-minus", type_ "checkbox", checked resetTime.isMinus, onCheck <| updateResetTimeMinus initialTimeSeconds ] []
            , text i18n.minusSign
            ]
        , input [ type_ "time", value <| resetTimeValueToString resetTime.timeValue, step "1", size 8, class "reset-form-time", onInput <| updateResetTimeValue resetTime.isMinus ] []
        , button [ class "contrast", class "reset-form-button", onClick Reset ]
            [ i [ class "fas", class "fa-backward-fast", class "button-icon" ] [], text i18n.reset ]
        ]


role : String -> Attribute Msg
role s =
    attribute "role" s


viewTimerSettings : I18n.Label -> Setting -> Html Msg
viewTimerSettings i18n setting =
    details [ class "settings", attribute "open" "true" ]
        [ summary [] [ text i18n.displaySetting ]
        , div [ class "grid" ]
            [ viewFgColorInput i18n setting.fgColor
            , viewFgFontInput i18n setting.fgFont
            ]
        , viewBgColorInput i18n setting.bgColor
        , div
            [ class "grid" ]
            [ label [ for "showHour" ]
                [ input [ type_ "checkbox", id "showHour", role "switch", checked setting.showHour, onClick ToggleShowHour ] []
                , text i18n.showHour
                ]
            , label [ for "showProgress" ]
                [ input [ type_ "checkbox", id "showProgress", role "switch", checked setting.showProgress, onClick ToggleShowProgress ] []
                , text i18n.showProgress
                ]
            ]
        ]


viewFgColorInput : I18n.Label -> String -> Html Msg
viewFgColorInput i18n fgColor =
    div [ class "grid" ]
        [ div []
            [ label [ for "fgColorPicker" ]
                [ text i18n.fgColor
                , input [ type_ "color", id "fgColorPicker", value fgColor, onInput SetFgColor ] []
                ]
            ]
        , div []
            [ label [ for "fgColorText" ]
                [ text i18n.fgColorRgb
                , input [ type_ "text", id "fgColorText", value fgColor, onInput SetFgColor ] []
                ]
            ]
        ]


viewFgFontInput : I18n.Label -> FgFont -> Html Msg
viewFgFontInput i18n font =
    div []
        [ label [ for "fgFont" ]
            [ text i18n.fgFont
            , small []
                [ a
                    [ href "https://docs.google.com/forms/d/e/1FAIpQLScMezFWGTb_kdzS8jPiP-bu3KpwwGGOv7HkSbRdQdEiIodUtA/viewform?usp=sf_link", class "secondary" ]
                    [ i [ class "fas", class "fa-up-right-from-square", role "button-icon" ] []
                    , text i18n.fgFontRequest
                    ]
                ]
            ]
        , select [ id "fgFont", onInput selectFgFont ]
            [ option [ value <| encodeFgFont DDinBold, selected <| font == DDinBold ] [ text i18n.fgFontDDinBold ]
            , option [ value <| encodeFgFont Lora, selected <| font == Lora ] [ text i18n.fgFontLora ]
            , option [ value <| encodeFgFont DSEG7ClassicBold, selected <| font == DSEG7ClassicBold ] [ text i18n.fgFontDSEG7ClassicBold ]
            ]
        ]


selectFgFont : String -> Msg
selectFgFont =
    decodeFgFont >> Maybe.map SetFgFont >> Maybe.withDefault NoOp


viewBgColorInput : I18n.Label -> BgColor -> Html Msg
viewBgColorInput i18n bgColor =
    div []
        [ label [ for "bgColor" ] [ text i18n.bgColor ]
        , select [ id "bgColor", onInput selectBgColor ]
            [ option [ value <| encodeBgColor GreenBack, selected <| bgColor == GreenBack ] [ text i18n.bgColorGB ]
            , option [ value <| encodeBgColor BlueBack, selected <| bgColor == BlueBack ] [ text i18n.bgColorBB ]
            , option [ value <| encodeBgColor Transparent, selected <| bgColor == Transparent ] [ text i18n.bgColorTP ]
            ]
        ]


selectBgColor : String -> Msg
selectBgColor =
    decodeBgColor >> Maybe.map SetBgColor >> Maybe.withDefault NoOp

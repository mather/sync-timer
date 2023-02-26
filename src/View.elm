module View exposing (DisplayTime, millisToDisplayTime, selectBgColor, selectFgFont, view)

import Html exposing (Attribute, Html, a, button, details, div, i, input, label, option, select, span, summary, text)
import Html.Attributes as A exposing (attribute, checked, class, for, id, selected, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Model exposing (BgColor(..), FgFont(..), Model, Setting, decodeBgColor, decodeFgFont, encodeFgFont)
import Msg exposing (Msg(..))


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


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewTimer model
        , viewTimerSettings model.setting
        ]


viewTimer : Model -> Html Msg
viewTimer model =
    div [ class "grid" ]
        [ viewTimerDigits model.timeMillis model.setting
        , viewTimerControls model
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
fontClass font =
    case font of
        DDinBold ->
            class "d-din-bold"

        Lora ->
            class "lora"


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


viewTimerControls : Model -> Html Msg
viewTimerControls model =
    div [ class "controls" ]
        [ div [] [ startPauseButton model.paused ]
        , div [ class "grid" ]
            [ div [] [ rewindButton ]
            , div [] [ fastForwardButton ]
            ]
        , div [] [ resetButton model.setting.initialTimeSeconds ]
        , div [] [ initialTimeSlider model.setting.initialTimeSeconds ]
        ]


startPauseButton : Bool -> Html Msg
startPauseButton paused =
    if paused then
        button
            [ onClick Start ]
            [ i [ class "fas", class "fa-play", class "button-icon" ] []
            , text "開始"
            ]

    else
        button
            [ onClick Pause, class "secondary" ]
            [ i [ class "fas", class "fa-pause", class "button-icon" ] []
            , text "一時停止"
            ]


rewindButton : Html Msg
rewindButton =
    button [ class "outline", onClick <| RewindSec 1 ]
        [ i [ class "fas", class "fa-backward", class "button-icon" ] []
        , text "1秒戻す"
        ]


fastForwardButton : Html Msg
fastForwardButton =
    button [ class "outline", onClick <| FastForwardSec 1 ]
        [ text "1秒進める"
        , i [ class "fas", class "fa-forward", class "button-icon" ] []
        ]


resetButton : Int -> Html Msg
resetButton initialTimeSeconds =
    button [ class "contrast", onClick Reset ]
        [ i [ class "fas", class "fa-backward-fast", class "button-icon" ] []
        , text <| String.fromInt initialTimeSeconds ++ " 秒にリセット"
        ]


role : String -> Attribute Msg
role s =
    attribute "role" s


viewTimerSettings : Setting -> Html Msg
viewTimerSettings setting =
    details [ class "settings", attribute "open" "true" ]
        [ summary [] [ text "表示設定" ]
        , div [ class "grid" ]
            [ viewFgColorInput setting.fgColor
            , viewFgFontInput setting.fgFont
            ]
        , viewBgColorInput setting.bgColor
        , div
            [ class "grid" ]
            [ label [ for "showHour" ]
                [ input [ type_ "checkbox", id "showHour", role "switch", checked setting.showHour, onClick ToggleShowHour ] []
                , text "1時間以上の動画を見る"
                ]
            , label [ for "showProgress" ]
                [ input [ type_ "checkbox", id "showProgress", role "switch", checked setting.showProgress, onClick ToggleShowProgress ] []
                , text "カウントダウンを視覚的に表現する"
                ]
            ]
        ]


viewFgColorInput : String -> Html Msg
viewFgColorInput fgColor =
    div [ class "grid" ]
        [ div []
            [ label [ for "fgColorPicker" ]
                [ text "文字色"
                , input [ type_ "color", id "fgColorPicker", value fgColor, onInput SetFgColor ] []
                ]
            ]
        , div []
            [ label [ for "fgColorText" ]
                [ text "文字色(RGB)"
                , input [ type_ "text", id "fgColorText", value fgColor, onInput SetFgColor ] []
                ]
            ]
        ]


viewFgFontInput : FgFont -> Html Msg
viewFgFontInput font =
    div []
        [ label [ for "fgFont" ] [ text "フォント" ]
        , select [ id "fgFont", onInput selectFgFont ]
            [ option [ value <| encodeFgFont DDinBold, selected <| font == DDinBold ] [ text "D-DIN bold (Sans Serif)" ]
            , option [ value <| encodeFgFont Lora, selected <| font == Lora ] [ text "Lora (Serif)" ]
            ]
        ]


selectFgFont : String -> Msg
selectFgFont =
    decodeFgFont >> Maybe.map SetFgFont >> Maybe.withDefault NoOp


viewBgColorInput : BgColor -> Html Msg
viewBgColorInput bgColor =
    div []
        [ label [ for "bgColor" ] [ text "背景色" ]
        , select [ id "bgColor", onInput selectBgColor ]
            [ option [ value "gb", selected <| bgColor == GreenBack ] [ text "グリーンバック(GB, #00ff00)" ]
            , option [ value "bb", selected <| bgColor == BlueBack ] [ text "ブルーバック(BB, #0000ff)" ]
            , option [ value "tp", selected <| bgColor == Transparent ] [ text "なし (White)" ]
            ]
        ]


selectBgColor : String -> Msg
selectBgColor =
    decodeBgColor >> Maybe.map SetBgColor >> Maybe.withDefault NoOp


initialTimeSlider : Int -> Html Msg
initialTimeSlider initialTimeSeconds =
    div [ class "delay-slider" ]
        [ input
            [ type_ "range"
            , A.min "-30"
            , A.max "30"
            , step "1"
            , onInput (String.toInt >> Maybe.withDefault 0 >> UpdateResetTime)
            , value <| String.fromInt initialTimeSeconds
            , tooltip <| "タイマーの開始時間: " ++ String.fromInt initialTimeSeconds ++ "秒"
            ]
            []
        ]


tooltip : String -> Attribute Msg
tooltip =
    attribute "data-tooltip"

port module Main exposing (DisplayTime, main, millisToDisplayTime)

import Browser
import Dict
import Html exposing (Attribute, Html, a, button, details, div, i, input, label, option, select, span, summary, text)
import Html.Attributes as A exposing (attribute, checked, class, disabled, for, id, selected, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as E
import Time
import Url.Builder as UB


type alias Model =
    { timeMillis : Int
    , paused : Bool
    , current : Maybe Time.Posix
    , setting : Setting
    }


type alias Setting =
    { bgColor : BgColor
    , fgColor : String
    , initialTimeSeconds : Int
    , showHour : Bool
    , showProgress : Bool
    }


defaultSetting : Setting
defaultSetting =
    { fgColor = "#415462"
    , bgColor = GreenBack
    , initialTimeSeconds = -10
    , showHour = True
    , showProgress = False
    }


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


type BgColor
    = Transparent
    | GreenBack
    | BlueBack


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
    let
        pairwise bgColor =
            ( encodeBgColor bgColor, bgColor )
    in
    Dict.fromList <| List.map pairwise [ GreenBack, BlueBack, Transparent ]


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


type alias SettingFromQuery =
    { fg : Maybe String
    , bg : Maybe String
    , init : Maybe Int
    , h : Maybe String
    , p : Maybe String
    }


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


parseSettingFromQuery : SettingFromQuery -> Setting
parseSettingFromQuery setting =
    { fgColor = setting.fg |> Maybe.withDefault defaultSetting.fgColor
    , bgColor = setting.bg |> Maybe.andThen decodeBgColor |> Maybe.withDefault defaultSetting.bgColor
    , initialTimeSeconds = setting.init |> Maybe.withDefault defaultSetting.initialTimeSeconds
    , showHour = setting.h |> Maybe.andThen decodeBoolean |> Maybe.withDefault defaultSetting.showHour
    , showProgress = setting.p |> Maybe.andThen decodeBoolean |> Maybe.withDefault defaultSetting.showProgress
    }


initialModel : SettingFromQuery -> ( Model, Cmd Msg )
initialModel setting =
    let
        initSetting =
            parseSettingFromQuery setting
    in
    ( { timeMillis = initSetting.initialTimeSeconds * 1000
      , paused = True
      , current = Nothing
      , setting = parseSettingFromQuery setting
      }
    , Cmd.none
    )


type Msg
    = Start
    | Pause
    | Reset
    | UpdateTime Int Time.Posix
    | UpdateResetTime Int
    | RewindSec Int
    | FastForwardSec Int
    | SetBgColor BgColor
    | SetFgColor String
    | ToggleShowHour
    | ToggleShowProgress
    | NoOp


urlFromSetting : Setting -> String
urlFromSetting setting =
    UB.toQuery
        [ UB.string "fg" setting.fgColor
        , UB.string "bg" <| encodeBgColor setting.bgColor
        , UB.int "init" setting.initialTimeSeconds
        , UB.string "h" <| encodeBoolean setting.showHour
        , UB.string "p" <| encodeBoolean setting.showProgress
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ setting } as model) =
    case msg of
        Start ->
            ( { model | paused = False }, timerStartEvent model.timeMillis )

        Pause ->
            ( { model | paused = True, current = Nothing }, timerPauseEvent model.timeMillis )

        Reset ->
            ( { model
                | timeMillis = model.setting.initialTimeSeconds * 1000
                , paused = True
                , current = Nothing
              }
            , timerResetEvent <| model.setting.initialTimeSeconds * 1000
            )

        UpdateTime millis current ->
            ( { model | timeMillis = millis, current = Just current }, Cmd.none )

        UpdateResetTime millis ->
            ( { model | setting = { setting | initialTimeSeconds = millis } }
            , setQueryString <| urlFromSetting { setting | initialTimeSeconds = millis }
            )

        RewindSec sec ->
            ( { model | timeMillis = model.timeMillis - sec * 1000 }, Cmd.none )

        FastForwardSec sec ->
            ( { model | timeMillis = model.timeMillis + sec * 1000 }, Cmd.none )

        SetBgColor bgColor ->
            ( { model | setting = { setting | bgColor = bgColor } }
            , setQueryString <| urlFromSetting { setting | bgColor = bgColor }
            )

        SetFgColor fgColor ->
            ( { model | setting = { setting | fgColor = fgColor } }
            , setQueryString <| urlFromSetting { setting | fgColor = fgColor }
            )

        ToggleShowHour ->
            ( { model | setting = { setting | showHour = not setting.showHour } }
            , setQueryString <| urlFromSetting { setting | showHour = not setting.showHour }
            )

        ToggleShowProgress ->
            ( { model | setting = { setting | showProgress = not setting.showProgress } }
            , setQueryString <| urlFromSetting { setting | showProgress = not setting.showProgress }
            )

        NoOp ->
            ( model, Cmd.none )


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
    div [ class "timer", timerBgColorClass setting.bgColor, style "color" setting.fgColor ]
        [ div [ class "digits" ] <|
            (List.concat <|
                [ [ span (styleTimerSign displayTime.isMinus) [ text "-" ] ]
                , List.foldr joinWithSegment [] <| List.map renderBig2Digits displaySegments
                ]
            )
                ++ viewProgressBar millis setting
        ]


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
        [ text <| String.fromInt initialTimeSeconds ++ " 秒にリセット" ]


role : String -> Attribute Msg
role s =
    attribute "role" s


viewTimerSettings : Setting -> Html Msg
viewTimerSettings setting =
    details [ class "settings", attribute "open" "true" ]
        [ summary [] [ text "表示設定" ]
        , div [ class "grid" ]
            [ viewFgColorInput setting.fgColor
            , viewBgColorInput setting.bgColor
            ]
        , div
            [ class "grid" ]
            [ label [ for "showHour" ]
                [ input [ type_ "checkbox", id "showHour", role "switch", checked setting.showHour, onClick ToggleShowHour ] []
                , text "時間を表示する"
                ]
            , label [ for "showProgress" ]
                [ input [ type_ "checkbox", id "showProgress", role "switch", checked setting.showProgress, onClick ToggleShowProgress ] []
                , text "カウントダウンをバーで表示する"
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
    flip Dict.get dictBgColor >> Maybe.map SetBgColor >> Maybe.withDefault NoOp


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


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tick now =
            UpdateTime (Time.posixToMillis now - (Time.posixToMillis <| Maybe.withDefault now model.current) + model.timeMillis) now
    in
    if model.paused then
        Sub.none

    else
        Time.every 33 tick


port setQueryString : String -> Cmd msg


port sendAnalyticsEvent : String -> Cmd msg


encodeAnalyticsEvent : String -> String -> String -> Maybe Int -> String
encodeAnalyticsEvent category action label value =
    E.encode 0 <|
        E.object <|
            [ ( "category", E.string category )
            , ( "action", E.string action )
            , ( "label", E.string label )
            ]
                ++ (Maybe.map (\v -> [ ( "value", E.int v ) ]) value |> Maybe.withDefault [])


formatTimeForAnalytics : Int -> String
formatTimeForAnalytics t =
    let
        dt =
            millisToDisplayTime t
    in
    [ if dt.isMinus then
        "-"

      else
        ""
    , String.fromInt dt.hours |> String.padLeft 2 '0'
    , ":"
    , String.fromInt dt.minutes |> String.padLeft 2 '0'
    , ":"
    , String.fromInt dt.seconds |> String.padLeft 2 '0'
    ]
        |> String.concat


timerStartEvent : Int -> Cmd msg
timerStartEvent currentTime =
    sendAnalyticsEvent <| encodeAnalyticsEvent "sync_timer" "sync_timer_start" (formatTimeForAnalytics currentTime) Nothing


timerPauseEvent : Int -> Cmd msg
timerPauseEvent currentTime =
    sendAnalyticsEvent <| encodeAnalyticsEvent "sync_timer" "sync_timer_pause" (formatTimeForAnalytics currentTime) Nothing


timerResetEvent : Int -> Cmd msg
timerResetEvent resetTime =
    sendAnalyticsEvent <| encodeAnalyticsEvent "sync_timer" "sync_timer_reset" (formatTimeForAnalytics resetTime) Nothing


main : Program SettingFromQuery Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

port module Main exposing (DisplayTime, main, millisToDisplayTime)

import Browser exposing (UrlRequest)
import Browser.Navigation
import Dict
import Html exposing (Attribute, Html, a, button, details, div, fieldset, footer, h1, h2, i, input, label, legend, li, main_, nav, p, small, span, summary, text, textarea, ul)
import Html.Attributes as A exposing (attribute, checked, class, for, href, id, name, readonly, rows, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as E
import Time
import Url
import Url.Builder as UB
import Url.Parser as UP
import Url.Parser.Query as Query


type alias Model =
    { timeMillis : Int
    , paused : Bool
    , current : Maybe Time.Posix
    , setting : Setting
    , key : Browser.Navigation.Key
    }


type alias Setting =
    { bgColor : BgColor
    , fgColor : String
    , initialTimeSeconds : Int
    , showHour : Bool
    }


defaultSetting : Setting
defaultSetting =
    { fgColor = "#415462"
    , bgColor = GreenBack
    , initialTimeSeconds = -10
    , showHour = True
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


encodeShowHour : Bool -> String
encodeShowHour b =
    if b then
        "true"

    else
        "false"


dictShowHour : Dict.Dict String Bool
dictShowHour =
    Dict.fromList [ ( "true", True ), ( "false", False ) ]


parserWithDefault : a -> Query.Parser (Maybe a) -> Query.Parser a
parserWithDefault default =
    Query.map <| Maybe.withDefault default


queryParser : Query.Parser Setting
queryParser =
    Query.map4
        Setting
        (parserWithDefault defaultSetting.bgColor <| Query.enum "bg" dictBgColor)
        (parserWithDefault defaultSetting.fgColor <| Query.string "fg")
        (parserWithDefault defaultSetting.initialTimeSeconds <| Query.int "init")
        (parserWithDefault defaultSetting.showHour <| Query.enum "h" dictShowHour)


initialModel : flag -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
initialModel _ url key =
    let
        urlParser =
            UP.query queryParser

        initSetting =
            UP.parse urlParser url |> Maybe.withDefault defaultSetting
    in
    ( { timeMillis = initSetting.initialTimeSeconds * 1000
      , paused = True
      , current = Nothing
      , setting = initSetting
      , key = key
      }
    , Cmd.none
    )


type Msg
    = Start
    | Pause
    | Reset
    | UpdateTime Int Time.Posix
    | UpdateResetTime Int
    | SetBgColor BgColor
    | SetFgColor String
    | ToggleShowHour
    | LinkClicked UrlRequest
    | NoOp


urlFromSetting : Setting -> String
urlFromSetting setting =
    UB.toQuery
        [ UB.string "fg" setting.fgColor
        , UB.string "bg" <| encodeBgColor setting.bgColor
        , UB.int "init" setting.initialTimeSeconds
        , UB.string "h" <| encodeShowHour setting.showHour
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
            , Browser.Navigation.replaceUrl model.key <|
                urlFromSetting { setting | initialTimeSeconds = millis }
            )

        SetBgColor bgColor ->
            ( { model | setting = { setting | bgColor = bgColor } }
            , Browser.Navigation.replaceUrl model.key <|
                urlFromSetting { setting | bgColor = bgColor }
            )

        SetFgColor fgColor ->
            ( { model | setting = { setting | fgColor = fgColor } }
            , Browser.Navigation.replaceUrl model.key <|
                urlFromSetting { setting | fgColor = fgColor }
            )

        ToggleShowHour ->
            ( { model | setting = { setting | showHour = not setting.showHour } }
            , Browser.Navigation.replaceUrl model.key <|
                urlFromSetting { setting | showHour = not setting.showHour }
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.load (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Sync Timer - 同時視聴用タイマー"
    , body =
        [ main_ [ class "container" ]
            [ viewHeader
            , viewTimer model
            , viewTimerSettings model.setting
            ]
        , viewFooter
        ]
    }


viewHeader : Html Msg
viewHeader =
    nav []
        [ ul []
            [ li []
                [ h1 [] [ text "Sync Timer" ] ]
            ]
        , ul []
            [ li []
                [ a [ href "/docs/", role "button", class "outline" ]
                    [ text "SyncTimerとは？"
                    ]
                ]
            ]
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
        (List.concat <|
            [ [ span (styleTimerSign displayTime.isMinus) [ text "-" ] ]
            , List.foldr joinWithSegment [] <| List.map renderBig2Digits displaySegments
            ]
        )


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
            [ onClick Pause ]
            [ i [ class "fas", class "fa-pause", class "button-icon" ] []
            , text "一時停止"
            ]


resetButton : Int -> Html Msg
resetButton initialTimeSeconds =
    button [ class "secondary", onClick Reset ]
        [ text <| String.fromInt initialTimeSeconds ++ " 秒にリセット" ]


role : String -> Attribute Msg
role s =
    attribute "role" s


viewTimerSettings : Setting -> Html Msg
viewTimerSettings setting =
    details [ class "settings", attribute "open" "true" ]
        [ summary [] [ text "表示設定" ]
        , div [ class "grid" ]
            [ div []
                [ label [ for "fgColorPicker" ]
                    [ text "文字色"
                    , input [ type_ "color", id "fgColorPicker", value setting.fgColor, onInput SetFgColor ] []
                    ]
                ]
            , div []
                [ label [ for "fgColorText" ]
                    [ text "文字色(RGB指定)"
                    , input [ type_ "text", id "fgColorText", value setting.fgColor, onInput SetFgColor ] []
                    ]
                ]
            ]
        , div []
            [ fieldset [ class "bgColor" ]
                [ legend [] [ text "背景色" ]
                , label [ for "greenback" ]
                    [ input [ type_ "radio", id "greenback", name "bgcolor", checked <| setting.bgColor == GreenBack, onClick <| SetBgColor GreenBack ] []
                    , text "GB"
                    ]
                , label [ for "blueback" ]
                    [ input [ type_ "radio", id "blueback", name "bgcolor", checked <| setting.bgColor == BlueBack, onClick <| SetBgColor BlueBack ] []
                    , text "BB"
                    ]
                , label [ for "transparent" ]
                    [ input [ type_ "radio", id "transparent", name "bgcolor", checked <| setting.bgColor == Transparent, onClick <| SetBgColor Transparent ] []
                    , text "なし"
                    ]
                ]
            ]
        , div
            []
            [ label [ for "showHour" ]
                [ input [ type_ "checkbox", id "showHour", role "switch", checked setting.showHour, onClick ToggleShowHour ] []
                , text "時間を表示する"
                ]
            ]
        ]


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


viewFooter : Html Msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ p []
                [ text "機能要望や質問・感想などは"
                , a [ A.href "https://docs.google.com/forms/d/e/1FAIpQLSfgmFqq-t-vv6gC1YpgoH3nCK1b7gI0ROC25K1NX9r5jGtndg/viewform?usp=sf_link", A.target "_blank", A.rel "noopener noreferrer" ] [ text "こちらのフォームからどうぞ" ]
                ]
            , div [] [ small [] [ text "よろしければ概要欄でタイマーの紹介をお願いします（必須ではありません）" ] ]
            , textarea [ readonly True, rows 2, id "introduce-request" ] [ text "同時視聴用タイマー SyncTimer を利用しています\nhttps://sync-timer.netlify.app/" ]
            , div [ class "grid" ]
                [ div [ class "footer-left" ]
                    [ h2 [] [ text "更新情報" ]
                    , ul []
                        [ li [] [ text "2022-11-18 時間の表示をON/OFFできるようにしました" ]
                        , li [] [ text "2022-11-14 YouTubeでの利用例を紹介しました" ]
                        ]
                    ]
                , div [ class "footer-right" ]
                    [ div []
                        [ a [ href "https://youtube.com/playlist?list=PLzz5NMXBDKoZ8UoGCgQI9mdcfmyjugbwz", A.target "_blank", A.rel "noopener noreferrer" ]
                            [ i [ class "fab", class "fa-youtube", class "button-icon" ] []
                            , text "YouTubeでの利用例"
                            ]
                        ]
                    , div []
                        [ a [ href "https://twitter.com/intent/user?user_id=62148177", A.target "_blank", A.rel "noopener noreferrer" ]
                            [ i [ class "fab", class "fa-twitter", class "button-icon" ] []
                            , text "@mather314 (開発者)"
                            ]
                        ]
                    , div []
                        [ a [ href "https://github.com/mather/sync-timer", class "secondary", A.target "_blank", A.rel "noopener noreferrer" ]
                            [ i [ class "fab", class "fa-github", class "button-icon" ] []
                            , text "mather/sync-timer"
                            ]
                        ]
                    , div [] [ text "© Eisuke Kuwahata" ]
                    ]
                ]
            ]
        ]


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


onUrlRequest : UrlRequest -> Msg
onUrlRequest =
    LinkClicked


onUrlChange : Url.Url -> Msg
onUrlChange _ =
    NoOp


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


main : Program () Model Msg
main =
    Browser.application
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }

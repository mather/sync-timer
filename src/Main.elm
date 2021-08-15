module Main exposing (DisplayTime, main, millisToDisplayTime)

import Browser
import Html exposing (Attribute, Html, a, article, button, details, div, footer, h1, h2, i, input, label, li, main_, nav, p, span, strong, summary, text, ul)
import Html.Attributes as A exposing (attribute, checked, class, for, href, id, name, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Time


type alias Model =
    { timeMillis : Int
    , paused : Bool
    , current : Maybe Time.Posix
    , initialTimeSeconds : Int
    , showHelp : Bool
    , setting : Setting
    }


type alias Setting =
    { bgColor : BgColor
    , fgColor : String
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


initialModel : flag -> ( Model, Cmd Msg )
initialModel _ =
    ( { timeMillis = -10000
      , paused = True
      , current = Nothing
      , initialTimeSeconds = -10
      , showHelp = False
      , setting =
            { bgColor = Transparent
            , fgColor = "#415462"
            }
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
    | ShowHelp Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | paused = False }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True, current = Nothing }, Cmd.none )

        Reset ->
            ( { model | timeMillis = model.initialTimeSeconds * 1000, paused = True, current = Nothing }, Cmd.none )

        UpdateTime millis current ->
            ( { model | timeMillis = millis, current = Just current }, Cmd.none )

        UpdateResetTime millis ->
            ( { model | initialTimeSeconds = millis }, Cmd.none )

        SetBgColor bgColor ->
            ( { model | setting = updateBgColor bgColor model.setting }, Cmd.none )

        SetFgColor fgColor ->
            ( { model | setting = updateFgColor fgColor model.setting }, Cmd.none )

        ShowHelp showHelp ->
            ( { model | showHelp = showHelp }, Cmd.none )


updateBgColor : BgColor -> Setting -> Setting
updateBgColor bgColor setting =
    { setting | bgColor = bgColor }


updateFgColor : String -> Setting -> Setting
updateFgColor fgColor setting =
    { setting | fgColor = fgColor }


view : Model -> Browser.Document Msg
view model =
    { title = "Sync Timer - 同時視聴用タイマー"
    , body =
        [ main_ [ class "container" ]
            [ viewHelpModal model.showHelp
            , viewHeader
            , viewTimerDigits model.timeMillis model.setting
            , viewTimerControls model
            , viewTimerSettings model.setting
            , viewFooter
            ]
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
                [ a [ class "twitter-share-icon", attribute "role" "button", href twitterIntentUrl, A.target "_blank", A.rel "noopener noreferrer" ]
                    [ i [ class "fab", class "fa-twitter", class "button-icon" ] []
                    , text "Share"
                    ]
                ]
            , li []
                [ a [ class "help-icon", onClick <| ShowHelp True ]
                    [ i [ class "fas", class "fa-question-circle" ] []
                    ]
                ]
            ]
        ]


twitterIntentUrl : String
twitterIntentUrl =
    "https://twitter.com/intent/tweet"
        ++ "?text=Sync+Timer+同時視聴配信用タイマー"
        ++ "&url=https%3A%2F%2Fmather.github.io%2Fsimple-stopwatch%2F"
        ++ "&via=mather314"


viewHelpModal : Bool -> Html Msg
viewHelpModal visible =
    let
        classes =
            if visible then
                [ style "display" "block" ]

            else
                []
    in
    div (id "help-modal" :: classes)
        [ div
            [ id "help-modal-bg" ]
            [ article [ id "help-content" ]
                [ h2 [] [ text "このサービスについて" ]
                , p [] [ text "同時視聴配信などでタイミングを合わせるためのタイマーとしてご利用いただけます" ]
                , ul []
                    [ li [] [ text "視聴者と同期させるためマイナスの秒数から開始できます(-30 〜 30)" ]
                    , li [] [ text "タイマー表示は固定幅です" ]
                    , li [] [ text "文字色、背景色(GB, BB)が変更できます" ]
                    ]
                , p [] [ text "不具合や改善要望などがあれば Twitter, Github へご連絡ください" ]
                , footer []
                    [ button [ onClick <| ShowHelp False ] [ text "閉じる" ]
                    ]
                ]
            ]
        ]


viewTimerDigits : Int -> Setting -> Html Msg
viewTimerDigits millis setting =
    let
        displayTime =
            millisToDisplayTime millis

        signVisibility =
            if displayTime.isMinus then
                "visible"

            else
                "hidden"
    in
    div [ class "timer", timerBgColorClass setting.bgColor, style "color" setting.fgColor ]
        (List.concat <|
            [ [ span (style "visibility" signVisibility :: styleBigDidits) [ text "-" ] ]
            , renderBig2Digits displayTime.hours
            , [ span styleBigDidits [ text ":" ] ]
            , renderBig2Digits displayTime.minutes
            , [ span styleBigDidits [ text ":" ] ]
            , renderBig2Digits displayTime.seconds
            ]
        )


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


styleBigDidits : List (Html.Attribute Msg)
styleBigDidits =
    [ class "digit" ]


renderBig2Digits : Int -> List (Html Msg)
renderBig2Digits digits =
    padZero 2 digits
        |> String.split ""
        |> List.map (\d -> span styleBigDidits [ text d ])


viewTimerControls : Model -> Html Msg
viewTimerControls model =
    div []
        [ div [] [ startPauseButton model.paused ]
        , div [ class "grid" ]
            [ div [] [ resetButton model.initialTimeSeconds ]
            , div [] [ initialTimeSlider model.initialTimeSeconds ]
            ]
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


viewTimerSettings : Setting -> Html Msg
viewTimerSettings setting =
    details [ class "settings" ]
        [ summary [] [ text "タイマーの表示設定" ]
        , div []
            [ strong [] [ text "文字色" ]
            , input [ type_ "color", id "fgColorPicker", value setting.fgColor, onInput SetFgColor ] []
            , input [ type_ "text", id "fgColorText", value setting.fgColor, onInput SetFgColor ] []
            ]
        , div []
            [ strong [] [ text "背景色" ]
            , input [ type_ "radio", id "transparent", name "bgcolor", checked <| setting.bgColor == Transparent, onClick <| SetBgColor Transparent ] []
            , label [ for "transparent" ] [ text "なし" ]
            , input [ type_ "radio", id "greenback", name "bgcolor", checked <| setting.bgColor == GreenBack, onClick <| SetBgColor GreenBack ] []
            , label [ for "greenback" ] [ text "グリーンバック" ]
            , input [ type_ "radio", id "blueback", name "bgcolor", checked <| setting.bgColor == BlueBack, onClick <| SetBgColor BlueBack ] []
            , label [ for "blueback" ] [ text "ブルーバック" ]
            ]
        ]


initialTimeSlider : Int -> Html Msg
initialTimeSlider initialTimeSeconds =
    div []
        [ input
            [ type_ "range"
            , A.min "-30"
            , A.max "30"
            , step "1"
            , class "delay-slider"
            , onInput (String.toInt >> Maybe.withDefault 0 >> UpdateResetTime)
            , value <| String.fromInt initialTimeSeconds
            , attribute "data-tooltip" "タイマーの開始時間"
            ]
            []
        ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ span [] [ text "© mather" ]
        , a [ href "https://twitter.com/mather314", attribute "role" "button", class "outline", A.target "_blank", A.rel "noopener noreferrer" ]
            [ i [ class "fab", class "fa-twitter", class "button-icon" ] []
            , text "mather314"
            ]
        , a [ href "https://github.com/mather/simple-stopwatch", attribute "role" "button", class "outline", class "secondary", A.target "_blank", A.rel "noopener noreferrer" ]
            [ i [ class "fab", class "fa-github", class "button-icon" ] []
            , text "mather"
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


main : Program () Model Msg
main =
    Browser.document
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

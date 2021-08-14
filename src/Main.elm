module Main exposing (DisplayTime, main, millisToDisplayTime)

import Browser
import Html exposing (Attribute, Html, a, button, details, div, footer, h1, i, input, li, main_, nav, span, summary, text, ul, strong, label)
import Html.Attributes as A exposing (attribute, class, href, name, step, style, type_, value, for, id, checked)
import Html.Events exposing (onClick, onInput)
import Time


type alias Model =
    { timeMillis : Int
    , paused : Bool
    , current : Maybe Time.Posix
    , initialTimeSeconds : Int
    , setting : Setting
    }


type alias Setting =
    { bgColor : BgColor
    }


type alias DisplayTime =
    { isMinus : Bool
    , hours : Int
    , minutes : Int
    , seconds : Int
    , milliSeconds : Int
    }


millisToDisplayTime : Int -> DisplayTime
millisToDisplayTime t =
    let
        absTime =
            abs t
    in
    { isMinus = t < 0
    , hours = absTime // 3600000
    , minutes = absTime // 60000 |> modBy 60
    , seconds = absTime // 1000 |> modBy 60
    , milliSeconds = modBy 1000 absTime
    }


type BgColor
    = Transparent
    | GreenBack
    | BlueBack


initialModel : flag -> ( Model, Cmd Msg )
initialModel _ =
    ( { timeMillis = 0
      , paused = True
      , current = Nothing
      , initialTimeSeconds = 0
      , setting =
            { bgColor = Transparent
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
            ( {model | setting = updateBgColor bgColor model.setting}, Cmd.none)

updateBgColor : BgColor -> Setting -> Setting
updateBgColor bgColor setting = 
    { setting | bgColor = bgColor }


view : Model -> Browser.Document Msg
view model =
    { title = "Simple Stopwatch"
    , body =
        [ main_ [ class "container" ]
            [ viewHeader
            , viewTimerDigits model.timeMillis model.setting.bgColor
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
            [ li [] [ h1 [] [ text "Simple Stopwatch" ] ] ]
        ]


viewTimerDigits : Int -> BgColor -> Html Msg
viewTimerDigits millis bgColor =
    let
        displayTime =
            millisToDisplayTime millis

        signVisibility =
            if displayTime.isMinus then
                "visible"

            else
                "hidden"
    in
    div [ class "timer", timerBgColorClass bgColor ]
        (List.concat
            [ [ span (style "visibility" signVisibility :: styleBigDidits) [ text "-" ] ]
            , renderBig2Digits displayTime.hours
            , [ span styleBigDidits [ text ":" ] ]
            , renderBig2Digits displayTime.minutes
            , [ span styleBigDidits [ text ":" ] ]
            , renderBig2Digits displayTime.seconds
            , [ span styleSmallDigits [ text "." ] ]
            , renderSmall3Digits displayTime.milliSeconds
            ]
        )

timerBgColorClass : BgColor -> Attribute Msg
timerBgColorClass bgColor = 
    case bgColor of
        Transparent -> class "transparent"
        GreenBack -> class "greenback"
        BlueBack -> class "blueback"

padZero : Int -> Int -> String
padZero wt digits =
    String.fromInt digits |> String.padLeft wt '0'


styleBigDidits : List (Html.Attribute Msg)
styleBigDidits =
    [ class "digit" ]


styleSmallDigits : List (Html.Attribute Msg)
styleSmallDigits =
    [ class "digit", class "small" ]


renderBig2Digits : Int -> List (Html Msg)
renderBig2Digits digits =
    padZero 2 digits
        |> String.split ""
        |> List.map (\d -> span styleBigDidits [ text d ])


renderSmall3Digits : Int -> List (Html Msg)
renderSmall3Digits digits =
    padZero 3 digits
        |> String.split ""
        |> List.map (\d -> span styleSmallDigits [ text d ])


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
        , div [ ]
            [ strong [] [ text "背景色" ]
            , input [ type_ "radio", id "transparent",  name "bgcolor", checked <| setting.bgColor == Transparent, onClick <| SetBgColor Transparent ] []
            , label [for "transparent"] [ text "なし" ]
            , input [ type_ "radio", id "greenback",  name "bgcolor", checked <| setting.bgColor == GreenBack, onClick <| SetBgColor GreenBack ] []
            , label [for "greenback"] [ text "グリーンバック" ]
            , input [ type_ "radio", id "blueback",  name "bgcolor", checked <| setting.bgColor == BlueBack, onClick <| SetBgColor BlueBack ] []
            , label [for "blueback"] [ text "ブルーバック" ]
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
        , a [ href "https://twitter.com/mather314", attribute "role" "button", class "outline" ]
            [ i [ class "fab", class "fa-twitter", class "button-icon" ] []
            , text "mather314"
            ]
        , a [ href "https://github.com/mather/simple-stopwatch", attribute "role" "button", class "outline", class "secondary" ]
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

module Main exposing (DisplayTime, main, millisToDisplayTime)

import Browser
import Html exposing (Html, a, button, div, footer, h1, i, input, li, main_, nav, span, text, ul)
import Html.Attributes as A exposing (attribute, class, href, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Time


type alias Model =
    { timeMillis : Int
    , paused : Bool
    , current : Maybe Time.Posix
    , setting : Setting
    }


type alias Setting =
    { initialTimeSeconds : Int
    , bgColor : BgColor
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
    = White
    | Green
    | Blue


initialModel : flag -> ( Model, Cmd Msg )
initialModel _ =
    ( { timeMillis = 0
      , paused = True
      , current = Nothing
      , setting =
            { initialTimeSeconds = 0
            , bgColor = White
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
            ( { model | timeMillis = model.setting.initialTimeSeconds * 1000, paused = True, current = Nothing }, Cmd.none )

        UpdateTime millis current ->
            ( { model | timeMillis = millis, current = Just current }, Cmd.none )

        UpdateResetTime millis ->
            ( { model | setting = setInitialTimeSeconds millis model.setting }, Cmd.none )


setInitialTimeSeconds : Int -> Setting -> Setting
setInitialTimeSeconds millis setting =
    { setting | initialTimeSeconds = millis }


view : Model -> Browser.Document Msg
view model =
    { title = "Simple Stopwatch"
    , body =
        [ main_ [ class "container" ]
            [ viewHeader
            , viewTimer model.timeMillis
            , viewButtons model
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


viewTimer : Int -> Html Msg
viewTimer millis =
    let
        displayTime =
            millisToDisplayTime millis

        signVisibility =
            if displayTime.isMinus then
                "visible"

            else
                "hidden"
    in
    div [ class "timer" ]
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


viewButtons : Model -> Html Msg
viewButtons model =
    div []
        [ div [] [ startPauseButton model.paused ]
        , div [ class "grid" ]
            [ div [] [ resetButton model.setting.initialTimeSeconds ]
            , div [] [ initialTimeSlider model.setting.initialTimeSeconds ]
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

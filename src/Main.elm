module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, a, button, div, footer, h1, i, input, nav, span, text)
import Html.Attributes as A exposing (class, href, max, min, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import String exposing (toInt)
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


classes : List String -> List (Attribute Msg)
classes xs =
    List.map class xs


view : Model -> Browser.Document Msg
view model =
    { title = "Simple Stopwatch"
    , body =
        [ div (classes [ "min-h-screen", "flex", "flex-col" ])
            [ viewHeader
            , viewTimer model.timeMillis
            , viewButtons model
            , viewFooter
            ]
        ]
    }


viewHeader : Html Msg
viewHeader =
    nav (classes [ "flex", "items-center", "bg-black", "p-4", "w-full" ])
        [ div (classes [ "flex", "items-center", "text-white" ])
            [ h1 (classes [ "text-2xl", "lg:text-3xl" ]) [ text "Simple Stopwatch" ] ]
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

        padZero =
            \w -> String.fromInt >> String.padLeft w '0'
    in
    div (classes [ "flex", "content-center", "flex-wrap", "justify-center", "h-56", "m-3" ])
        [ div (classes [ "text-center", "text-5xl", "sm:text-6xl", "font-mono" ])
            [ span [ style "visibility" signVisibility ] [ text "-" ]
            , span [] [ text <| padZero 2 displayTime.hours ]
            , span [] [ text ":" ]
            , span [] [ text <| padZero 2 displayTime.minutes ]
            , span [] [ text ":" ]
            , span [] [ text <| padZero 2 displayTime.seconds ]
            , span (classes [ "text-3xl", "sm:text-4xl" ]) [ text "." ]
            , span (classes [ "text-3xl", "sm:text-4xl" ]) [ text <| padZero 3 displayTime.milliSeconds ]
            ]
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    div (classes [ "flex-grow", "grid", "grid-cols-1", "sm:grid-cols-2", "lg:grid-cols-4", "gap-3", "m-3" ])
        [ div (classes [ "col-span-1", "sm:col-span-2" ]) [ startPauseButton model.paused ]
        , div (classes [ "col-span-1" ]) [ resetButton model.setting.initialTimeSeconds ]
        , div (classes [ "col-span-1" ]) [ initialTimeSlider model.setting.initialTimeSeconds ]
        ]


startPauseButton : Bool -> Html Msg
startPauseButton paused =
    if paused then
        button 
            (classes [ "btn", "bg-green-500", "hover:bg-green-400", "rounded-lg", "w-full", "p-2", "text-white", "shadow-lg" ] ++ [ onClick Start ])
            [ i (classes ["fas", "fa-play", "mr-2"]) []
            , text "開始"
            ]

    else
        button 
            (classes [ "btn", "bg-yellow-600", "hover:bg-yellow-500", "rounded-lg", "w-full", "p-2", "text-white", "shadow-lg" ] ++ [ onClick Pause ])
            [ i (classes ["fas", "fa-pause", "mr-2"]) []
            , text "一時停止"
            ]


resetButton : Int -> Html Msg
resetButton initialTimeSeconds =
    button (classes [ "btn", "bg-red-700", "hover:bg-red-500", "text-white", "rounded-lg", "w-full", "p-2", "shadow-lg" ] ++ [ onClick Reset ]) [ text <| String.fromInt initialTimeSeconds ++ "秒にリセット" ]


initialTimeSlider : Int -> Html Msg
initialTimeSlider initialTimeSeconds =
    div (classes [ "flex", "items-center", "justify-left", "h-full" ])
        [ div (classes [ "text-right" ]) [ text "-30秒" ]
        , div (classes [ "mx-2" ])
            [ input
                [ type_ "range"
                , A.min "-30"
                , A.max "30"
                , step "1"
                , onInput (String.toInt >> Maybe.withDefault 0 >> UpdateResetTime)
                , value <| String.fromInt initialTimeSeconds
                ]
                []
            ]
        , div (classes [ "text-left" ]) [ text "30秒" ]
        ]


viewFooter : Html Msg
viewFooter =
    footer (classes [ "flex", "flex-wrap", "justify-center", "my-3", "items-center" ])
        [ div (classes [ "m-2" ]) [ text "© mather" ]
        , div (classes [ "m-2" ])
            [ a [ href "https://twitter.com/mather314" ]
                [ button (classes [ "btn", "bg-blue-500", "rounded", "px-3", "py-2", "text-white" ])
                    [ i (classes [ "fab", "fa-twitter", "mr-1" ]) []
                    , text "mather314"
                    ]
                ]
            ]
        , div (classes [ "m-2" ])
            [ a [ href "https://github.com/mather/simple-stopwatch" ]
                [ button (classes [ "btn", "bg-black", "rounded", "px-3", "py-2", "text-white" ])
                    [ i (classes [ "fab", "fa-github", "mr-1" ]) []
                    , text "mather"
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


main : Program () Model Msg
main =
    Browser.document
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

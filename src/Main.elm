module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, nav, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Material.Button as Button
import Material.FormField as FormField
import Material.LayoutGrid as LayoutGrid
import Material.Slider as Slider
import Material.Theme as Theme
import Material.TopAppBar as TopAppBar
import Material.Typography as Typography
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
        [ div []
            [ viewHeader
            , viewTimer model.timeMillis
            , viewButtons model
            ]
        ]
    }


viewHeader : Html Msg
viewHeader =
    TopAppBar.regular (TopAppBar.config |> TopAppBar.setFixed True)
        [ TopAppBar.row []
            [ TopAppBar.section
                [ TopAppBar.alignStart ]
                [ span [ TopAppBar.title ] [ text "Simple Stopwatch" ] ]
            ]
        ]


viewTimer : Int -> Html Msg
viewTimer millis =
    let
        signVisibility = 
            if millis < 0 then
                "visible"
            else
                "hidden"

        unsignedMillis =
            abs millis

        hours =
            unsignedMillis // 3600000 |> String.fromInt |> String.padLeft 2 '0'

        minutes =
            unsignedMillis // 60000 |> modBy 60 |> String.fromInt |> String.padLeft 2 '0'

        seconds =
            unsignedMillis // 1000 |> modBy 60 |> String.fromInt |> String.padLeft 2 '0'

        milliSeconds =
            modBy 1000 unsignedMillis |> String.fromInt |> String.padLeft 3 '0'
    in
    div [ TopAppBar.fixedAdjust ]
        [ LayoutGrid.layoutGrid []
            [ LayoutGrid.inner []
                [ LayoutGrid.cell [ LayoutGrid.span1Phone, LayoutGrid.span2Tablet, LayoutGrid.span3Desktop ] []
                , LayoutGrid.cell [ LayoutGrid.span4Tablet, LayoutGrid.span6Desktop, Typography.headline3, LayoutGrid.alignMiddle ]
                    [ div [style "padding" "50px 0" ] 
                        [ span [style "visibility" signVisibility ] [text "-"]
                        , span [] [text <|  hours ++ ":" ++ minutes ++ ":" ++ seconds ++ "." ++ milliSeconds ]
                        ]
                    ]
                , LayoutGrid.cell [ LayoutGrid.span1Phone, LayoutGrid.span2Tablet, LayoutGrid.span3Desktop ] []
                ]
            ]
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    LayoutGrid.layoutGrid []
        [ LayoutGrid.inner []
            [ LayoutGrid.cell [ LayoutGrid.span4Phone, LayoutGrid.span8Tablet, LayoutGrid.span6Desktop ] [ startPauseButton model.paused ]
            , LayoutGrid.cell [ LayoutGrid.span2Phone, LayoutGrid.span4Tablet, LayoutGrid.span3Desktop ] [ resetButton model.setting.initialTimeSeconds ]
            , LayoutGrid.cell [ LayoutGrid.span2Phone, LayoutGrid.span4Tablet, LayoutGrid.span3Desktop ] [ initialTimeSlider model.setting.initialTimeSeconds ]
            ]
        ]

startPauseButton : Bool -> Html Msg
startPauseButton paused =
    if paused then
        Button.raised
            (Button.config
                |> Button.setOnClick Start
                |> Button.setIcon (Just "play_arrow")
                |> Button.setAttributes [ colorSuccess, style "width" "100%" ]
            )
            "開始"

    else
        Button.raised
            (Button.config
                |> Button.setOnClick Pause
                |> Button.setIcon (Just "pause")
                |> Button.setAttributes [ colorWarning, style "width" "100%" ]
            )
            "一時停止"


resetButton : Int -> Html Msg
resetButton initialTimeSeconds = 
    Button.raised
        (Button.config
            |> Button.setOnClick Reset
            --|> Button.setIcon (Just "clear")
            |> Button.setAttributes [ colorError, style "width" "100%" ]
        )
        <| String.fromInt initialTimeSeconds ++ "秒にリセット"


colorSuccess : Html.Attribute Msg
colorSuccess =
    style "background-color" "#28a745"


colorWarning : Html.Attribute Msg
colorWarning =
    style "background-color" "#eb9e05"


colorError : Html.Attribute Msg
colorError =
    style "background-color" "#dc3545"



initialTimeSlider : Int -> Html Msg
initialTimeSlider initialTimeSeconds =
    Slider.slider
        (Slider.config
            |> Slider.setMin (Just -30)
            |> Slider.setMax (Just 30)
            |> Slider.setStep (Just 1)
            |> Slider.setDiscrete True
            |> Slider.setValue (Just <| toFloat initialTimeSeconds)
            |> Slider.setOnInput (round >> UpdateResetTime)
        )


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

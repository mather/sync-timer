module Main exposing (main)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import Tailwind exposing (tailwind, withClasses)
import Tailwind.Classes exposing (..)
import Time


type alias Model =
    { timeMillis : Int
    , paused : Bool
    , current : Maybe Time.Posix
    , resetTimeMillis : Int
    }


initialModel : flag -> ( Model, Cmd Msg )
initialModel _ =
    ( { timeMillis = 0
      , paused = True
      , current = Nothing
      , resetTimeMillis = 0
      }
    , Cmd.none
    )


type Msg
    = Start
    | Pause
    | Reset
    | UpdateTime Int Time.Posix


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
            ( { model | timeMillis = 0, paused = True, current = Nothing }, Cmd.none )

        UpdateTime millis current ->
            ( { model | timeMillis = millis, current = Just current }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Timer"
    , body =
        [ div [ tailwind [ min_h_screen, flex, flex_col, text_center ] ]
            [ viewTimer model.timeMillis
            , viewButtons model
            ]
        ]
    }


viewTimer : Int -> Html Msg
viewTimer millis =
    let
        sign =
            if millis < 0 then
                "-"

            else
                " "

        hours =
            millis // 3600000 |> String.fromInt |> String.padLeft 2 '0'

        minutes =
            millis // 60000 |> modBy 60 |> String.fromInt |> String.padLeft 2 '0'

        seconds =
            millis // 1000 |> modBy 60 |> String.fromInt |> String.padLeft 2 '0'

        milliSeconds =
            modBy 1000 millis |> String.fromInt |> String.padLeft 3 '0'
    in
        div [ tailwind [ m_10, px_20, py_10, font_sans, font_extrabold, text_5xl, text_left, border ] ]
        [ span [ tailwind [w_auto] ] [ text <| sign ++ hours ++ ":" ++ minutes ++ ":" ++ seconds ++ "." ++ milliSeconds ]
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    div [ tailwind [ flex, justify_center, my_4 ] ]
        [ startPauseButton model.paused
        , div [ tailwind [ flex_grow, text_right, mx_2, items_center ] ]
            [ text "秒前からスタート"
            ]
        , button
            [ tailwind <|
                withClasses [ "bg-red-500", "hover:bg-red-700" ] <|
                    [ text_white, font_bold, mx_2, py_2, px_4, rounded ]
            , onClick Reset
            ]
            [ text "Reset" ]
        ]

startPauseButton : Bool -> Html Msg
startPauseButton paused =
    if paused then
        button
            [ tailwind <|
                withClasses [ "bg-blue-500", "hover:bg-blue-700" ] <|
                    [ text_white, font_bold, mx_2, py_2, px_4, rounded, w_1over4 ]
            , onClick Start
            ]
            [ text "Start" ]
    else
        button
            [ tailwind <|
                withClasses [ "bg-orange-500", "hover:bg-orange-700" ] <|
                    [ text_white, font_bold, mx_2, py_2, px_4, rounded, w_1over4 ]
            , onClick Pause
            ]
            [ text "Pause" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tick now =
            UpdateTime (Time.posixToMillis now - (Time.posixToMillis <| Maybe.withDefault now model.current) + model.timeMillis) now
    in
    if model.paused then
        Sub.none

    else
        Time.every 67 tick


main : Program () Model Msg
main =
    Browser.document
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

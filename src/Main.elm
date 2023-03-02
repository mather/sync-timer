module Main exposing (..)

import Browser
import Model exposing (Model, Setting, decodeBgColor, decodeBoolean, decodeFgFont, defaultSetting)
import Msg exposing (Msg(..), update)
import Time
import View exposing (view)


{-| flag: SettingFromQuery
-}
type alias SettingFromQuery =
    { fg : Maybe String
    , bg : Maybe String
    , ff : Maybe String
    , init : Maybe Int
    , h : Maybe String
    , p : Maybe String
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


parseSettingFromQuery : SettingFromQuery -> Setting
parseSettingFromQuery setting =
    { fgColor = setting.fg |> Maybe.withDefault defaultSetting.fgColor
    , bgColor = setting.bg |> Maybe.andThen decodeBgColor |> Maybe.withDefault defaultSetting.bgColor
    , fgFont = setting.ff |> Maybe.andThen decodeFgFont |> Maybe.withDefault defaultSetting.fgFont
    , initialTimeSeconds = setting.init |> Maybe.withDefault defaultSetting.initialTimeSeconds
    , showHour = setting.h |> Maybe.andThen decodeBoolean |> Maybe.withDefault defaultSetting.showHour
    , showProgress = setting.p |> Maybe.andThen decodeBoolean |> Maybe.withDefault defaultSetting.showProgress
    }


calculateMillis : Maybe Time.Posix -> Int -> Time.Posix -> Int
calculateMillis prevTime prevMillis currentTime =
    case prevTime of
        Just prev ->
            prevMillis + (Time.posixToMillis currentTime - Time.posixToMillis prev)

        Nothing ->
            prevMillis


tick : Model -> Time.Posix -> Msg
tick model currentTime =
    UpdateTime (calculateMillis model.current model.timeMillis currentTime) currentTime


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Time.every 33 <| tick model


main : Program SettingFromQuery Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

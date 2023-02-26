module Main exposing (..)

import Browser
import Model exposing (Model, Setting, decodeBgColor, decodeBoolean, decodeFgFont, defaultSetting)
import Msg exposing (Msg(..), update)
import Time
import View exposing (view)


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


main : Program SettingFromQuery Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

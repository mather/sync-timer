port module Msg exposing (Msg(..), update)

import Analytics exposing (timerFastForwardEvent, timerPauseEvent, timerResetEvent, timerRewindEvent, timerStartEvent)
import Dict
import Model exposing (BgColor, FgFont, Model, Setting, defaultSetting, encodeBgColor, encodeBoolean, encodeFgFont)
import Time
import Url.Builder as UB


port setQueryString : String -> Cmd msg


type Msg
    = Start
    | Pause
    | Reset
    | UpdateTime Time.Posix
    | UpdateResetTime Int
    | RewindSec Int
    | FastForwardSec Int
    | SetBgColor BgColor
    | SetFgColor String
    | SetFgFont FgFont
    | ToggleShowHour
    | ToggleShowProgress
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ setting } as model) =
    case msg of
        Start ->
            ( { model | paused = False }, timerStartEvent model.timeMillis setting )

        Pause ->
            ( { model | paused = True, current = Nothing }, timerPauseEvent model.timeMillis setting )

        Reset ->
            ( { model
                | timeMillis = model.setting.initialTimeSeconds * 1000
                , paused = True
                , current = Nothing
              }
            , timerResetEvent model.timeMillis setting
            )

        UpdateTime current ->
            ( { model
                | timeMillis = calculateMillis model.current model.timeMillis current
                , current = Just current
              }
            , Cmd.none
            )

        UpdateResetTime seconds ->
            ( { model | setting = { setting | initialTimeSeconds = seconds } }
            , setQueryString <| urlFromSetting { setting | initialTimeSeconds = seconds }
            )

        RewindSec sec ->
            ( { model | timeMillis = model.timeMillis - sec * 1000 }, timerRewindEvent model.timeMillis setting )

        FastForwardSec sec ->
            ( { model | timeMillis = model.timeMillis + sec * 1000 }, timerFastForwardEvent model.timeMillis setting )

        SetBgColor bgColor ->
            ( { model | setting = { setting | bgColor = bgColor } }
            , setQueryString <| urlFromSetting { setting | bgColor = bgColor }
            )

        SetFgColor fgColor ->
            ( { model | setting = { setting | fgColor = fgColor } }
            , setQueryString <| urlFromSetting { setting | fgColor = fgColor }
            )

        SetFgFont fgFont ->
            ( { model | setting = { setting | fgFont = fgFont } }
            , setQueryString <| urlFromSetting { setting | fgFont = fgFont }
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


calculateMillis : Maybe Time.Posix -> Int -> Time.Posix -> Int
calculateMillis prevTime prevMillis currentTime =
    case prevTime of
        Just prev ->
            prevMillis + (Time.posixToMillis currentTime - Time.posixToMillis prev)

        Nothing ->
            prevMillis


settingToDict : Setting -> Dict.Dict String String
settingToDict setting =
    Dict.fromList
        [ ( "fg", setting.fgColor )
        , ( "bg", setting.bgColor |> encodeBgColor )
        , ( "ff", setting.fgFont |> encodeFgFont )
        , ( "init", setting.initialTimeSeconds |> String.fromInt )
        , ( "h", setting.showHour |> encodeBoolean )
        , ( "p", setting.showProgress |> encodeBoolean )
        ]


defaultSettingDict : Dict.Dict String String
defaultSettingDict =
    settingToDict defaultSetting


isNotDefault : String -> String -> Bool
isNotDefault key value =
    Dict.get key defaultSettingDict
        |> Maybe.map ((==) value >> not)
        |> Maybe.withDefault False


diffSettingDict : Setting -> Dict.Dict String String
diffSettingDict setting =
    settingToDict setting
        |> Dict.filter isNotDefault


urlFromSetting : Setting -> String
urlFromSetting setting =
    diffSettingDict setting
        |> Dict.toList
        |> List.map (\( k, v ) -> UB.string k v)
        |> UB.toQuery

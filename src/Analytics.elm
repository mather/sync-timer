port module Analytics exposing (..)

import Json.Encode as E
import Model exposing (Setting, encodeBgColor, encodeFgFont)


{-| for analytics
-}
port sendAnalyticsEvent : String -> Cmd msg


encodeAnalyticsEvent : String -> String -> String -> Setting -> String
encodeAnalyticsEvent category action label setting =
    E.encode 0 <|
        E.object <|
            [ ( "category", E.string category )
            , ( "action", E.string action )
            , ( "label", E.string label )
            , ( "setting_fgColor", E.string setting.fgColor )
            , ( "setting_bgColor", E.string <| encodeBgColor setting.bgColor )
            , ( "setting_fgFont", E.string <| encodeFgFont setting.fgFont )
            , ( "setting_initial", E.int setting.initialTimeSeconds )
            , ( "setting_show_hours", E.bool setting.showHour )
            , ( "setting_show_progress", E.bool setting.showProgress )
            ]


formatTimeForAnalytics : Int -> String
formatTimeForAnalytics t =
    let
        absMilliSeconds =
            abs t

        actualTime =
            { isMinus = t < 0
            , hours = absMilliSeconds // 3600000
            , minutes = absMilliSeconds // 60000 |> modBy 60
            , seconds = absMilliSeconds // 1000 |> modBy 60
            , milliSeconds = modBy 1000 absMilliSeconds
            }
    in
    [ if actualTime.isMinus then
        "-"

      else
        ""
    , String.fromInt actualTime.hours |> String.padLeft 2 '0'
    , ":"
    , String.fromInt actualTime.minutes |> String.padLeft 2 '0'
    , ":"
    , String.fromInt actualTime.seconds |> String.padLeft 2 '0'
    , "."
    , String.fromInt actualTime.milliSeconds |> String.padLeft 3 '0'
    ]
        |> String.concat


sendEvent : String -> Int -> Setting -> Cmd msg
sendEvent action currentTime setting =
    sendAnalyticsEvent <| encodeAnalyticsEvent "sync_timer" action (formatTimeForAnalytics currentTime) setting


timerStartEvent : Int -> Setting -> Cmd msg
timerStartEvent =
    sendEvent "sync_timer_start"


timerPauseEvent : Int -> Setting -> Cmd msg
timerPauseEvent =
    sendEvent "sync_timer_pause"


timerRewindEvent : Int -> Setting -> Cmd msg
timerRewindEvent =
    sendEvent "sync_timer_rewind"


timerFastForwardEvent : Int -> Setting -> Cmd msg
timerFastForwardEvent =
    sendEvent "sync_timer_fastforward"


timerResetEvent : Int -> Setting -> Cmd msg
timerResetEvent =
    sendEvent "sync_timer_reset"

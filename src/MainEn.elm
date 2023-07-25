module MainEn exposing (..)

import Browser
import Flag exposing (SettingFromQuery)
import I18n
import Model exposing (Model)
import Msg exposing (Msg(..), update)
import Subscriptions exposing (subscriptions)
import View exposing (view)


main : Program SettingFromQuery Model Msg
main =
    Browser.element
        { init = Flag.initialModel
        , view = view I18n.labelEn
        , update = update
        , subscriptions = subscriptions
        }

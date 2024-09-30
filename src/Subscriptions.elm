module Subscriptions exposing (..)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Time.every (1000 / 60) UpdateTime

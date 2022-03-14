module Main exposing (DisplayTime, main, millisToDisplayTime)

import Browser exposing (UrlRequest)
import Browser.Navigation
import Dict
import Html exposing (Attribute, Html, a, article, button, div, footer, h1, h2, header, i, iframe, input, label, li, main_, nav, node, p, span, strong, text, ul)
import Html.Attributes as A exposing (attribute, checked, class, for, height, href, id, name, src, step, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Time
import Url
import Url.Builder as UB
import Url.Parser as UP
import Url.Parser.Query as Query


type alias Model =
    { timeMillis : Int
    , paused : Bool
    , current : Maybe Time.Posix
    , initialTimeSeconds : Int
    , showHelp : Bool
    , setting : Setting
    , key : Browser.Navigation.Key
    }


type alias Setting =
    { bgColor : BgColor
    , fgColor : String
    }


type alias DisplayTime =
    { isMinus : Bool
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


millisToDisplayTime : Int -> DisplayTime
millisToDisplayTime t =
    let
        absSeconds =
            toFloat t / 1000 |> floor |> abs
    in
    { isMinus = t < 0
    , hours = absSeconds // 3600
    , minutes = absSeconds // 60 |> modBy 60
    , seconds = absSeconds |> modBy 60
    }


type BgColor
    = Transparent
    | GreenBack
    | BlueBack


type alias InitParams =
    { fgColor : Maybe String
    , bgColor : Maybe BgColor
    , initialTimeSeconds : Maybe Int
    }


queryParser : Query.Parser InitParams
queryParser =
    Query.map3
        InitParams
        (Query.string "fg")
        (Query.enum "bg" <| Dict.fromList [ ( "gb", GreenBack ), ( "bb", BlueBack ), ( "tp", Transparent ) ])
        (Query.int "init")


parser : UP.Parser (InitParams -> a) a
parser =
    UP.query queryParser


initialModel : flag -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
initialModel _ url key =
    let
        initParams =
            UP.parse parser url

        initialTimeSeconds =
            initParams |> Maybe.andThen (\p -> p.initialTimeSeconds) |> Maybe.withDefault -10

        initialBgColor =
            initParams |> Maybe.andThen (\p -> p.bgColor) |> Maybe.withDefault GreenBack

        initialFgColor =
            initParams |> Maybe.andThen (\p -> p.fgColor) |> Maybe.withDefault "#415462"
    in
    ( { timeMillis = initialTimeSeconds * 1000
      , paused = True
      , current = Nothing
      , initialTimeSeconds = initialTimeSeconds
      , showHelp = False
      , setting =
            { bgColor = initialBgColor
            , fgColor = initialFgColor
            }
      , key = key
      }
    , Cmd.none
    )


type Msg
    = Start
    | Pause
    | Reset
    | UpdateTime Int Time.Posix
    | UpdateResetTime Int
    | SetBgColor BgColor
    | SetFgColor String
    | ShowHelp Bool
    | NoOp


bgString : BgColor -> String
bgString bg =
    case bg of
        GreenBack ->
            "gb"

        BlueBack ->
            "bb"

        Transparent ->
            "tp"


urlFromConfig : String -> BgColor -> Int -> String
urlFromConfig fg bg initialTimeSeconds =
    UB.toQuery [ UB.string "fg" fg, UB.string "bg" <| bgString bg, UB.int "init" initialTimeSeconds ]


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
            ( { model | timeMillis = model.initialTimeSeconds * 1000, paused = True, current = Nothing }, Cmd.none )

        UpdateTime millis current ->
            ( { model | timeMillis = millis, current = Just current }, Cmd.none )

        UpdateResetTime millis ->
            ( { model | initialTimeSeconds = millis }
            , Browser.Navigation.replaceUrl model.key <| urlFromConfig model.setting.fgColor model.setting.bgColor millis
            )

        SetBgColor bgColor ->
            ( { model | setting = updateBgColor bgColor model.setting }
            , Browser.Navigation.replaceUrl model.key <| urlFromConfig model.setting.fgColor bgColor model.initialTimeSeconds
            )

        SetFgColor fgColor ->
            ( { model | setting = updateFgColor fgColor model.setting }
            , Browser.Navigation.replaceUrl model.key <| urlFromConfig fgColor model.setting.bgColor model.initialTimeSeconds
            )

        ShowHelp showHelp ->
            ( { model | showHelp = showHelp }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateBgColor : BgColor -> Setting -> Setting
updateBgColor bgColor setting =
    { setting | bgColor = bgColor }


updateFgColor : String -> Setting -> Setting
updateFgColor fgColor setting =
    { setting | fgColor = fgColor }


view : Model -> Browser.Document Msg
view model =
    { title = "Sync Timer - 同時視聴用タイマー"
    , body =
        [ main_ [ class "container" ]
            [ viewHeader
            , viewTimer model
            , viewTimerControls model
            , viewFooter
            ]
        , viewHelpModal model.showHelp
        ]
    }


role : String -> Attribute Msg
role =
    attribute "role"


viewHeader : Html Msg
viewHeader =
    nav []
        [ ul []
            [ li []
                [ h1 [] [ text "Sync Timer" ] ]
            ]
        , ul []
            [ li []
                [ button [ onClick <| ShowHelp True ]
                    [ i [ class "fas", class "fa-question-circle", class "button-icon" ] []
                    , text "ヘルプ"
                    ]
                ]
            ]
        ]


twitterIntentUrl : String
twitterIntentUrl =
    UB.crossOrigin "https://twitter.com"
        [ "intent", "tweet" ]
        [ UB.string "text" "Sync Timer 同時視聴配信用タイマー"
        , UB.string "url" "https://sync-timer.netlify.app/"
        , UB.string "via" "mather314"
        ]


dialog : Bool -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
dialog isOpen attributes =
    if isOpen then
        node "dialog" <| attribute "open" "" :: attributes

    else
        node "dialog" attributes


viewHelpModal : Bool -> Html Msg
viewHelpModal visible =
    dialog visible
        []
        [ article []
            [ header []
                [ a [ attribute "aria-label" "Close", class "close", onClick <| ShowHelp False ] []
                , text "SyncTimerとは？"
                ]
            , p [] [ text "YouTube配信などで視聴者と同じ動画を同時視聴するときに動画の開始時間や視聴タイミングを合わせるためのタイマーです" ]
            , p [] [ text "使い方を動画にまとめましたので、参考にしてください" ]
            , iframe [ id "ytplayer", type_ "text/html", width 640, height 360, src "https://www.youtube.com/embed/m1Basm-TqGU?rel=0" ] []
            , p [] [ text "不具合や改善要望などがあれば Twitter, Github へご連絡ください" ]
            , footer []
                [ button [ onClick <| ShowHelp False ] [ text "閉じる" ] ]
            ]
        ]


viewTimer : Model -> Html Msg
viewTimer model =
    div [ class "grid" ]
        [ viewTimerDigits model.timeMillis model.setting
        , viewTimerSettings model.setting
        ]


viewTimerDigits : Int -> Setting -> Html Msg
viewTimerDigits millis setting =
    let
        displayTime =
            millisToDisplayTime millis

        signVisibility =
            if displayTime.isMinus then
                "visible"

            else
                "hidden"
    in
    div [ class "timer", timerBgColorClass setting.bgColor, style "color" setting.fgColor ]
        (List.concat <|
            [ [ span (style "visibility" signVisibility :: styleTimerDigit) [ text "-" ] ]
            , renderBig2Digits displayTime.hours
            , [ span styleTimerSep [ text ":" ] ]
            , renderBig2Digits displayTime.minutes
            , [ span styleTimerSep [ text ":" ] ]
            , renderBig2Digits displayTime.seconds
            ]
        )


timerBgColorClass : BgColor -> Attribute Msg
timerBgColorClass bgColor =
    case bgColor of
        Transparent ->
            class "transparent"

        GreenBack ->
            class "greenback"

        BlueBack ->
            class "blueback"


padZero : Int -> Int -> String
padZero wt digits =
    String.fromInt digits |> String.padLeft wt '0'


styleTimerSep : List (Html.Attribute Msg)
styleTimerSep =
    [ class "sep" ]


styleTimerDigit : List (Html.Attribute Msg)
styleTimerDigit =
    [ class "digit" ]


renderBig2Digits : Int -> List (Html Msg)
renderBig2Digits digits =
    padZero 2 digits
        |> String.split ""
        |> List.map (\d -> span styleTimerDigit [ text d ])


viewTimerControls : Model -> Html Msg
viewTimerControls model =
    div []
        [ div [] [ startPauseButton model.paused ]
        , div [ class "grid" ]
            [ div [] [ resetButton model.initialTimeSeconds ]
            , div [] [ initialTimeSlider model.initialTimeSeconds ]
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


viewTimerSettings : Setting -> Html Msg
viewTimerSettings setting =
    div [ class "settings" ]
        [ div []
            [ strong [] [ text "文字色" ]
            , input [ type_ "color", id "fgColorPicker", value setting.fgColor, onInput SetFgColor ] []
            , input [ type_ "text", id "fgColorText", value setting.fgColor, onInput SetFgColor ] []
            ]
        , div []
            [ strong [] [ text "背景色" ]
            , input [ type_ "radio", id "greenback", name "bgcolor", checked <| setting.bgColor == GreenBack, onClick <| SetBgColor GreenBack ] []
            , label [ for "greenback" ] [ text "GB" ]
            , input [ type_ "radio", id "blueback", name "bgcolor", checked <| setting.bgColor == BlueBack, onClick <| SetBgColor BlueBack ] []
            , label [ for "blueback" ] [ text "BB" ]
            , input [ type_ "radio", id "transparent", name "bgcolor", checked <| setting.bgColor == Transparent, onClick <| SetBgColor Transparent ] []
            , label [ for "transparent" ] [ text "なし" ]
            ]
        ]


initialTimeSlider : Int -> Html Msg
initialTimeSlider initialTimeSeconds =
    div [ class "delay-slider" ]
        [ input
            [ type_ "range"
            , A.min "-30"
            , A.max "30"
            , step "1"
            , onInput (String.toInt >> Maybe.withDefault 0 >> UpdateResetTime)
            , value <| String.fromInt initialTimeSeconds
            , tooltip <| "タイマーの開始時間: " ++ String.fromInt initialTimeSeconds ++ "秒"
            ]
            []
        ]


tooltip : String -> Attribute Msg
tooltip =
    attribute "data-tooltip"


viewFooter : Html Msg
viewFooter =
    footer []
        [ span [] [ text "© Eisuke Kuwahata" ]
        , a [ href "https://twitter.com/intent/user?user_id=62148177", role "button", A.target "_blank", A.rel "noopener noreferrer", tooltip "更新情報をTwitterでつぶやくこともあります" ]
            [ i [ class "fab", class "fa-twitter", class "button-icon" ] []
            , text "@mather314"
            ]
        , a [ role "button", class "outline", href twitterIntentUrl, A.target "_blank", A.rel "noopener noreferrer", tooltip "拡散希望" ]
            [ i [ class "fab", class "fa-twitter", class "button-icon" ] []
            , text "シェアする"
            ]
        , a [ href "https://github.com/mather/simple-stopwatch", role "button", class "secondary", A.target "_blank", A.rel "noopener noreferrer", tooltip "Issue, PRをお待ちしています" ]
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


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url.Url -> Msg
onUrlChange _ =
    NoOp


main : Program () Model Msg
main =
    Browser.application
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }

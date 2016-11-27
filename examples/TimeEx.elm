module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr exposing (..)
import Time exposing (Time, second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : Time
    , paused : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 False, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | TogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            if model.paused then
                ( model, Cmd.none )
            else
                ( { model | time = newTime }, Cmd.none )

        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        angle =
            turns (Time.inMinutes model.time)

        handX =
            toString (50 + 40 * cos angle)

        handY =
            toString (50 + 40 * sin angle)

        angleHour =
            turns (Time.inHours model.time)

        handXHour =
            toString (50 + 20 * cos angleHour)

        handYHour =
            toString (50 + 20 * sin angleHour)

        buttonText =
            if model.paused then
                "resume"
            else
                "pause"
    in
        Html.div []
            [ Html.div []
                [ Html.text <| toString <| model
                , Html.button [ onClick TogglePause ] [ Html.text buttonText ]
                ]
            , Svg.svg [ SvgAttr.viewBox "0 0 100 100", SvgAttr.width "300px" ]
                [ Svg.circle [ SvgAttr.cx "50", SvgAttr.cy "50", SvgAttr.r "45", SvgAttr.fill "#0B79CE" ] []
                , Svg.line [ SvgAttr.x1 "50", SvgAttr.y1 "50", SvgAttr.x2 handX, SvgAttr.y2 handY, SvgAttr.stroke "#023963" ] []
                , Svg.line [ SvgAttr.x1 "50", SvgAttr.y1 "50", SvgAttr.x2 handXHour, SvgAttr.y2 handYHour, SvgAttr.stroke "#630239" ] []
                ]
            ]

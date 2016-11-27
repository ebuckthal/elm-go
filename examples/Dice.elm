module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Random exposing (..)
import Json.Decode as Json
import Http


--


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
--}



-- MODEL


type alias Model =
    { topic : String
    , gifUrl : String
    }


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)



-- UPDATE


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

        request =
            Http.get url decodeGifUrl
    in
        Http.send NewGif request


decodeGifUrl : Json.Decoder String
decodeGifUrl =
    Json.at [ "data", "image_url" ] Json.string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        NewGif (Ok newUrl) ->
            ( { model | gifUrl = newUrl }, Cmd.none )

        NewGif (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ img [ src model.gifUrl ] []
        , button [ onClick MorePlease ] [ text "gif more!" ]
        ]



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "cats" "waiting.gif", Cmd.none )

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { input : String
    , messages : List String
    }


type Msg
    = Input String
    | Send
    | NewMessage String


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { input, messages } =
    case msg of
        Input newInput ->
            ( Model newInput messages, Cmd.none )

        Send ->
            ( Model "" messages, WebSocket.send "ws://echo.websocket.org" input )

        NewMessage str ->
            ( Model input (List.append messages [ str ]), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://echo.websocket.org" NewMessage



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewMessage model.messages)
        , input [ type_ "text", onInput Input ] []
        , button [ onClick Send ] [ text "send" ]
        ]


viewMessage : String -> Html Msg
viewMessage message =
    div [] [ text message ]



{--
view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        , input [ placeholder "Text to reverse", onInput Change ] []
        , div [] [ text (String.reverse model.title) ]
        , input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
        , viewValidation model
        , button [ onClick Roll ] [ text "roll!" ]
        ]


viewValidation : Model -> Html msg
viewValidation model =
    let
        ( color, message ) =
            if model.password == model.passwordAgain then
                ( "green", "OK" )
            else
                ( "red", "Passwords do not match!" )
    in
        div [ style [ ( "color", color ) ] ] [ text message ]
--}
-- SUBSCRIPTIONS
{--
subscriptions : Model -> Sub Msg
subscriptions model =
--}

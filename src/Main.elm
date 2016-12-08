module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (..)
import Svg as Svg
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import List
import String
import Time exposing (Time, second)
import Matrix exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { turn : Team
    , mouseOver : Maybe Location
    , board : Matrix Point
    }


type Team
    = WhiteTeam
    | BlackTeam


type Point
    = Empty
    | White
    | Black


type Cardinal
    = Up
    | Down
    | Left
    | Right



-- UPDATE


type Msg
    = Reset
    | Tick Time
    | SetPoint Location
    | SetMouseOver (Maybe Location)
    | Pass


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        Tick newTime ->
            ( model, Cmd.none )

        Pass ->
            let
                nextTeam =
                    if (model.turn == WhiteTeam) then
                        BlackTeam
                    else
                        WhiteTeam
            in
                ( { model | turn = nextTeam }, Cmd.none )

        SetMouseOver location ->
            ( { model | mouseOver = location }, Cmd.none )

        SetPoint location ->
            let
                newBoard =
                    if (model.turn == WhiteTeam) then
                        (set location White model.board)
                    else
                        (set location Black model.board)

                nextTeam =
                    if (model.turn == WhiteTeam) then
                        BlackTeam
                    else
                        WhiteTeam
            in
                ( { model | board = newBoard, turn = nextTeam, mouseOver = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ (renderGo model)
        , button [ Html.Events.onClick Reset ] [ text "reset" ]
        , button [ Html.Events.onClick Pass ] [ text "pass" ]
        ]


renderGo : Model -> Html Msg
renderGo model =
    let
        patterns =
            Svg.defs
                []
                [ Svg.pattern
                    [ Svg.Attributes.id "lines"
                    , Svg.Attributes.x "0"
                    , Svg.Attributes.y "0"
                    , Svg.Attributes.width "6"
                    , Svg.Attributes.height "6"
                    , Svg.Attributes.patternUnits "userSpaceOnUse"
                    ]
                    [ Svg.polygon [ Svg.Attributes.points "5 0 6 0 0 6 0 5" ] []
                    , Svg.polygon [ Svg.Attributes.points "6 5 6 6 5 6" ] []
                    ]
                ]

        bg =
            Svg.rect
                [ Svg.Attributes.width "100"
                , Svg.Attributes.height "100"
                , fill "url(#lines)"
                , fillOpacity "0.05"
                ]
                []
    in
        Svg.svg
            [ viewBox "0 0 100 100"
            , Svg.Attributes.width "400px"
            ]
            (List.concat
                [ [ patterns ]
                , [ bg ]
                , (flatten (mapWithLocation (renderPoint model) model.board))
                ]
            )


renderPiece : Model -> Location -> Point -> Svg.Svg Msg
renderPiece model loc point =
    let
        pointCenter =
            (100 / (toFloat (colCount model.board)) / 2)

        pointWidth =
            pointCenter * 0.9

        pieceAttributes =
            if (point == White) then
                [ strokeWidth "0.5", stroke "black", fill "white" ]
            else if (point == Black) then
                [ strokeWidth "0.5", stroke "black", fill "black" ]
            else
                [ fill "transparent"
                , Svg.Events.onMouseOver (SetMouseOver (Just loc))
                , Svg.Events.onMouseOut (SetMouseOver Nothing)
                , Svg.Events.onClick (SetPoint loc)
                ]

        hoverAttributes =
            case model.mouseOver of
                Nothing ->
                    []

                Just mouseOver ->
                    if (mouseOver == loc) then
                        case model.turn of
                            WhiteTeam ->
                                [ fill "white", stroke "red", strokeWidth "0.7", cursor "pointer" ]

                            BlackTeam ->
                                [ fill "black", stroke "red", strokeWidth "0.7", cursor "pointer" ]
                    else
                        []
    in
        Svg.circle
            (List.concat
                [ [ cx <| toString <| pointCenter
                  , cy <| toString <| pointCenter
                  , r <| toString <| pointWidth
                  ]
                , pieceAttributes
                , hoverAttributes
                ]
            )
            []


renderLine : Cardinal -> Float -> Svg.Svg Msg
renderLine card pointSize =
    let
        center =
            toString (pointSize / 2)

        max =
            toString pointSize

        attrs =
            if (card == Left) then
                [ x1 "0", y1 center, x2 center, y2 center ]
            else if (card == Right) then
                [ x1 center, y1 center, x2 max, y2 center ]
            else if (card == Up) then
                [ x1 center, y1 "0", x2 center, y2 center ]
            else
                [ x1 center, y1 max, x2 center, y2 center ]
    in
        Svg.line (List.append attrs [ stroke "black", strokeWidth "0.5" ]) []


renderPoint : Model -> Location -> Point -> Svg.Svg Msg
renderPoint model loc point =
    let
        pointSize =
            (100 / (toFloat (colCount model.board)))

        transformString =
            ("translate("
                ++ (toString ((toFloat (Matrix.col loc)) * pointSize))
                ++ ","
                ++ (toString ((toFloat (Matrix.row loc)) * pointSize))
                ++ ")"
            )

        upline =
            if (Matrix.row loc > 0) then
                [ renderLine Up pointSize ]
            else
                []

        leftline =
            if (Matrix.col loc > 0) then
                [ renderLine Left pointSize ]
            else
                []

        rightline =
            if (Matrix.col loc < ((Matrix.colCount model.board) - 1)) then
                [ renderLine Right pointSize ]
            else
                []

        downline =
            if (Matrix.row loc < (Matrix.rowCount model.board) - 1) then
                [ renderLine Down pointSize ]
            else
                []
    in
        Svg.g
            [ Svg.Attributes.width <| toString <| pointSize
            , Svg.Attributes.height <| toString <| pointSize
            , transform transformString
            ]
            (List.concat
                [ upline, downline, leftline, rightline, [ renderPiece model loc point ] ]
            )



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model WhiteTeam Nothing (square 7 (\location -> Empty)), Cmd.none )

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html exposing (beginnerProgram)
import Html.Events exposing (..)
import List


-- lession 1


politely : String -> String
politely phrase =
    "Excuse me, " ++ phrase


ask : String -> String -> String
ask thing place =
    "is there a "
        ++ thing
        ++ " in the "
        ++ place
        ++ "?"


askPolitelyAboutFish : String -> String
askPolitelyAboutFish =
    politely << (ask "fish")



-- lession 2


type alias Dog =
    { name : String
    , age : Int
    }


dog =
    { name = "Inca"
    , age = 3
    }


renderDog : Dog -> String
renderDog dog =
    dog.name ++ ", " ++ (toString dog.age)


foo =
    [ "one", "two", "three" ]



-- lession 3


type alias Person =
    { name : String
    , age : Int
    }


people =
    [ { name = "Legolas", age = 2931 }
    , { name = "Gimli", age = 139 }
    ]


findPerson : String -> List Person -> Maybe Person
findPerson name peeps =
    List.foldl
        (\peep memo ->
            case memo of
                Just _ ->
                    memo

                Nothing ->
                    if peep.name == name then
                        Just peep
                    else
                        Nothing
        )
        Nothing
        peeps



-- lession 4


type alias Ship =
    { name : String
    , model : String
    , cost : Int
    }


ships =
    [ { name = "X-wing", cost = 149999 }
    , { name = "Millennium Falcon", cost = 100000 }
    , { name = "Death Star", cost = 1000000000000 }
    ]


renderShip ship =
    li []
        [ text ship.name
        , text ", "
        , b []
            [ text <| toString ship.cost ]
        ]


renderShips ships =
    div
        [ style
            [ ( "font-family", "-apple-system" )
            , ( "padding", "1em" )
            ]
        ]
        [ h1 [] [ text "Ships" ]
        , ul [] (List.map renderShip ships)
        ]



-- lession 5


numbers =
    [ 1, 2, 3, 4, 5 ]


printThing : thing -> Html msg
printThing thing =
    ul [] [ text <| toString thing ]


fruits =
    [ { name = "Orange" }, { name = "Banana" } ]



-- main =
--     ul [] (List.map printThing numbers)
-- lession 6


model =
    { showFace = False }


type Msg
    = ShowFace


update msg model_ =
    case msg of
        ShowFace ->
            { model_ | showFace = True }


view model_ =
    div []
        [ h1 [] [ text "Face generator" ]
        , button [ onClick ShowFace ] [ text "Face me" ]
        , if model_.showFace then
            text ":)"
          else
            text ""
        ]


main =
    beginnerProgram
        { model = model
        , update = update
        , view = view
        }

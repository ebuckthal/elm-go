module Main exposing (..)

import Html exposing (..)


pluralize : String -> String -> Int -> String
pluralize singular plural length =
    if length > 1 then
        plural
    else
        singular


items =
    [ "Green Eggs", "Green Ham" ]


main =
    div []
        [ h1 [] [ text <| (pluralize "Item" "Items" (List.length items)) ]
        , text <| toString <| items
        ]

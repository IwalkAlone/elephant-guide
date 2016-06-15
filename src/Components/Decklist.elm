module Components.Decklist exposing (..)

import Dict exposing (..)


type alias ID =
    Int


type alias Decklist =
    Dict ID Int


cardCount : Decklist -> Int
cardCount decklist =
    decklist |> Dict.values |> List.sum


slotValue : Decklist -> ID -> Int
slotValue decklist cardId =
    case Dict.get cardId decklist of
        Nothing ->
            0

        Just value ->
            value

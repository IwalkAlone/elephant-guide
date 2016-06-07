port module Ports exposing (..)

import Components.Archetype as Archetype exposing (..)
import Components.Card as Card exposing (..)


type alias ID =
    Int


type alias SavedDeckModel =
    { archetypes : List ( ID, Archetype.Model )
    , cards : List ( ID, Card.Model )
    , slots : List ( ( ID, ID ), Int )
    , maindeck : List ( ID, Int )
    , sideboard : List ( ID, Int )
    , nextId : ID
    }


port saveDeck : SavedDeckModel -> Cmd msg


port loadDeck : (SavedDeckModel -> msg) -> Sub msg

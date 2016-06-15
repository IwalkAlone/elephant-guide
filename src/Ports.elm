port module Ports exposing (..)

import Components.Archetype as Archetype exposing (..)
import Components.Card as Card exposing (..)


type alias ID =
    Int


type alias SavedDeckModel =
    { archetypes : List ( ID, SavedArchetypeModel )
    , cards : List ( ID, Card.Model )
    , maindeck : List ( ID, Int )
    , sideboard : List ( ID, Int )
    , nextId : ID
    }


type alias SavedArchetypeModel =
    { name : String
    , weight : Float
    , decklist : List ( ID, Int )
    }


port saveDeck : SavedDeckModel -> Cmd msg


port loadDeck : (SavedDeckModel -> msg) -> Sub msg

port module Ports exposing (..)

import Components.Card as Card exposing (..)


type alias ID =
    Int


type alias SavedDeckModel =
    { archetypes : List SavedArchetypeModel
    , cards : List Card.Model
    , maindeck : List ( ID, Int )
    , sideboard : List ( ID, Int )
    , nextId : ID
    }


type alias SavedArchetypeModel =
    { id : Int
    , name : String
    , weight : Float
    , decklist : List ( ID, Int )
    }


port saveDeck : SavedDeckModel -> Cmd msg


port loadDeck : (SavedDeckModel -> msg) -> Sub msg

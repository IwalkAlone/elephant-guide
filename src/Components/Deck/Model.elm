module Components.Deck.Model exposing (..)

import ID exposing (..)
import Components.Archetype as Archetype
import Components.Card as Card
import Components.Decklist as Decklist exposing (..)
import Dict
import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing (..)
import Ports exposing (TableMetrics)


type alias Model =
    { archetypes : List Archetype.Model
    , cards : List Card.Model
    , maindeck : Decklist
    , sideboard : Decklist
    , nextId : ID
    , cardIndexBeingDragged : Maybe Int
    , tableMetrics :
        Maybe TableMetrics
        --    , dragState : DragState
    , dragInsertAtIndex : Maybe Int
    }


type DragState
    = NotDragging
    | Dragging Int Int


initialModel : Model
initialModel =
    { archetypes = []
    , cards = []
    , maindeck = Dict.empty
    , sideboard = Dict.empty
    , nextId = 1
    , cardIndexBeingDragged = Nothing
    , tableMetrics = Nothing
    , dragInsertAtIndex = Nothing
    }


encoder : Model -> JE.Value
encoder model =
    JE.object
        [ ( "archetypes", JE.list (List.map Archetype.encoder model.archetypes) )
        , ( "cards", JE.list (List.map Card.encoder model.cards) )
        , ( "nextId", JE.int model.nextId )
        , ( "maindeck", Decklist.encoder model.maindeck )
        , ( "sideboard", Decklist.encoder model.sideboard )
        ]


decoder : Decoder Model
decoder =
    JD.object8 Model
        ("archetypes" := JD.list Archetype.decoder)
        ("cards" := JD.list Card.decoder)
        ("maindeck" := Decklist.decoder)
        ("sideboard" := Decklist.decoder)
        ("nextId" := JD.int)
        (JD.succeed Nothing)
        (JD.succeed Nothing)
        (JD.succeed Nothing)

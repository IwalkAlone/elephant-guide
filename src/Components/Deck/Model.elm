module Components.Deck.Model exposing (..)

import ID exposing (..)
import Components.Archetype as Archetype
import Components.Card as Card
import Components.Decklist as Decklist exposing (..)
import Dict
import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (required, hardcoded, decode)
import TableMetrics exposing (..)


type alias Model =
    { archetypes : List Archetype.Model
    , cards : List Card.Model
    , maindeck : Decklist
    , sideboard : Decklist
    , nextId : ID
    , tableMetrics :
        Maybe TableMetrics
    , dragState : DragState
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
    , tableMetrics = Nothing
    , dragState = NotDragging
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
    decode Model
        |> required "archetypes" (JD.list Archetype.decoder)
        |> required "cards" (JD.list Card.decoder)
        |> required "maindeck" Decklist.decoder
        |> required "sideboard" Decklist.decoder
        |> required "nextId" JD.int
        |> hardcoded Nothing
        |> hardcoded NotDragging

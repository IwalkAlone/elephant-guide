module Components.Deck.Update exposing (..)

import Components.Deck.Model as Model exposing (..)
import Components.Archetype as Archetype
import Components.Card as Card
import ID exposing (..)
import Dict
import DOM exposing (Rectangle)


type DecklistKind
    = ArchetypeList ID
    | Maindeck
    | Sideboard


type Msg
    = AddArchetype
    | DeleteArchetype ID
    | AddCard
    | EditSlot DecklistKind ID Int
    | DragStart ID Rectangle
    | ArchetypeMsg ID Archetype.Msg
    | CardMsg ID Card.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddArchetype ->
            { model
                | archetypes = model.archetypes ++ [ { id = model.nextId, name = "New Archetype", weight = 0, decklist = Dict.empty } ]
                , nextId = model.nextId + 1
            }

        DeleteArchetype id ->
            { model
                | archetypes = List.filter (\archetype -> archetype.id /= id) model.archetypes
            }

        AddCard ->
            { model
                | cards = model.cards ++ [ Card.initialModel model.nextId "New Card" ]
                , nextId = model.nextId + 1
            }

        EditSlot decklistKind cardId newValue ->
            case decklistKind of
                ArchetypeList archetypeId ->
                    let
                        updateArchetype archetype =
                            if archetype.id == archetypeId then
                                { archetype | decklist = Dict.insert cardId newValue archetype.decklist }
                            else
                                archetype
                    in
                        { model | archetypes = List.map updateArchetype model.archetypes }

                Maindeck ->
                    { model | maindeck = Dict.insert cardId newValue model.maindeck }

                Sideboard ->
                    { model | sideboard = Dict.insert cardId newValue model.sideboard }

        ArchetypeMsg id msg ->
            let
                updateArchetype archetype =
                    if archetype.id == id then
                        Archetype.update msg archetype
                    else
                        archetype
            in
                { model | archetypes = List.map updateArchetype model.archetypes }

        CardMsg id msg ->
            let
                updateCard card =
                    if card.id == id then
                        Card.update msg card
                    else
                        card
            in
                { model | cards = List.map updateCard model.cards }

        DragStart id rect ->
            let
                debug =
                    Debug.log "id, rect" ( id, rect )
            in
                model

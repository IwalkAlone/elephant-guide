module Components.Deck.Update exposing (..)

import Components.Deck.Model as Model exposing (..)
import Components.Archetype as Archetype
import Components.Card as Card
import ID exposing (..)
import Dict
import Ports
import Mouse
import Task exposing (Task)
import Json.Encode as JE
import Json.Decode as JD
import Http


type DecklistKind
    = ArchetypeList ID
    | Maindeck
    | Sideboard


type Msg
    = AddArchetype
    | DeleteArchetype ID
    | AddCard
    | EditSlot DecklistKind ID Int
    | DragStart ID
    | DragMove Mouse.Position
    | DragEnd Mouse.Position
    | ReceivedTableMetrics Ports.TableMetrics
    | ArchetypeMsg ID Archetype.Msg
    | CardMsg ID Card.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddArchetype ->
            { model
                | archetypes = model.archetypes ++ [ { id = model.nextId, name = "New Archetype", weight = 0, decklist = Dict.empty } ]
                , nextId = model.nextId + 1
            }
                ! [ saveDeck model ]

        DeleteArchetype id ->
            { model
                | archetypes = List.filter (\archetype -> archetype.id /= id) model.archetypes
            }
                ! [ saveDeck model ]

        AddCard ->
            { model
                | cards = model.cards ++ [ Card.initialModel model.nextId "New Card" ]
                , nextId = model.nextId + 1
            }
                ! [ saveDeck model ]

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
                        { model | archetypes = List.map updateArchetype model.archetypes } ! [ saveDeck model ]

                Maindeck ->
                    { model | maindeck = Dict.insert cardId newValue model.maindeck } ! [ saveDeck model ]

                Sideboard ->
                    { model | sideboard = Dict.insert cardId newValue model.sideboard } ! [ saveDeck model ]

        ArchetypeMsg id msg ->
            let
                updateArchetype archetype =
                    if archetype.id == id then
                        Archetype.update msg archetype
                    else
                        archetype
            in
                { model | archetypes = List.map updateArchetype model.archetypes } ! [ saveDeck model ]

        CardMsg id msg ->
            let
                updateCard card =
                    if card.id == id then
                        Card.update msg card
                    else
                        card
            in
                { model | cards = List.map updateCard model.cards } ! [ saveDeck model ]

        DragStart id ->
            model ! [ Ports.requestTableMetrics () ]

        ReceivedTableMetrics metrics ->
            { model | tableMetrics = Just metrics } ! []

        DragMove position ->
            { model | dragInsertAtIndex = dragInsertAtIndex model position } ! []

        DragEnd position ->
            { model | tableMetrics = Nothing } ! []

        NoOp ->
            model ! []


dragInsertAtIndex : Model -> Mouse.Position -> Maybe Int
dragInsertAtIndex model position =
    case model.tableMetrics of
        Nothing ->
            Nothing

        Just metrics ->
            List.filter (\item -> item < toFloat position.y) metrics.rowBottoms |> List.length |> Just


saveDeck : Model -> Cmd Msg
saveDeck model =
    let
        saveArchetype archetype =
            { id = archetype.id
            , weight = archetype.weight
            , name = archetype.name
            , decklist = Dict.toList archetype.decklist
            }

        saveDeckModel =
            { archetypes = List.map saveArchetype model.archetypes
            , cards = model.cards
            , nextId = model.nextId
            , maindeck = Dict.toList model.maindeck
            , sideboard = Dict.toList model.sideboard
            }
    in
        Cmd.batch [ Task.perform (always NoOp) (always NoOp) (postDeck model), Ports.saveDeck saveDeckModel ]


postDeck : Model -> Task Http.Error String
postDeck model =
    Http.post JD.string "http://localhost:3000/save" (Model.encoder model |> (JE.encode 4) |> Http.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.tableMetrics of
        Nothing ->
            Ports.receiveTableMetrics ReceivedTableMetrics

        Just _ ->
            Sub.batch ([ Mouse.moves DragMove, Mouse.ups DragEnd ])

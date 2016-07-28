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
    | DragStart Int
    | DragMove Mouse.Position
    | DragEnd Mouse.Position
    | ReceivedTableMetrics Ports.TableMetrics
    | FocusAndSelect String
    | ArchetypeMsg ID Archetype.Msg
    | CardMsg ID Card.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddArchetype ->
            withSave
                { model
                    | archetypes = model.archetypes ++ [ { id = model.nextId, name = "New Archetype", weight = 0, decklist = Dict.empty } ]
                    , nextId = model.nextId + 1
                }

        DeleteArchetype id ->
            withSave
                { model
                    | archetypes = List.filter (\archetype -> archetype.id /= id) model.archetypes
                }

        AddCard ->
            withSave
                { model
                    | cards = model.cards ++ [ Card.initialModel model.nextId "New Card" ]
                    , nextId = model.nextId + 1
                }

        EditSlot decklistKind cardId newValue ->
            let
                newModel =
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
            in
                withSave newModel

        ArchetypeMsg id msg ->
            let
                updateArchetype archetype =
                    if archetype.id == id then
                        Archetype.update msg archetype
                    else
                        archetype
            in
                withSave { model | archetypes = List.map updateArchetype model.archetypes }

        CardMsg id msg ->
            let
                updateCard card =
                    if card.id == id then
                        Card.update msg card
                    else
                        card
            in
                withSave { model | cards = List.map updateCard model.cards }

        DragStart index ->
            { model | dragState = Dragging index index } ! [ Ports.requestTableMetrics () ]

        ReceivedTableMetrics metrics ->
            { model | tableMetrics = Just metrics } ! []

        DragMove position ->
            -- important not to modify model if possible because of lazy optimization
            case model.dragState of
                NotDragging ->
                    model ! []

                Dragging fromIndex toIndex ->
                    let
                        maybeNewToIndex =
                            dragInsertAtIndex model position
                    in
                        case maybeNewToIndex of
                            Nothing ->
                                model ! []

                            Just newToIndex ->
                                (if newToIndex == toIndex then
                                    model
                                 else
                                    { model | dragState = Dragging fromIndex newToIndex }
                                )
                                    ! []

        DragEnd position ->
            case model.dragState of
                NotDragging ->
                    model ! []

                Dragging fromIndex toIndex ->
                    { model | cards = splice1 fromIndex toIndex model.cards, tableMetrics = Nothing, dragState = NotDragging } ! []

        FocusAndSelect elementId ->
            model ! [ Ports.focusAndSelect elementId ]

        NoOp ->
            model ! []


withSave : Model -> ( Model, Cmd Msg )
withSave model =
    ( model, saveDeck model )


dragInsertAtIndex : Model -> Mouse.Position -> Maybe Int
dragInsertAtIndex model position =
    case model.tableMetrics of
        Nothing ->
            Nothing

        Just metrics ->
            List.filter (\item -> item < toFloat position.y) metrics.rowBottoms |> List.length |> Just


splice1 : Int -> Int -> List a -> List a
splice1 fromIndex toIndex list =
    if fromIndex == toIndex then
        list
    else
        let
            listContainingItem =
                List.take 1 (List.drop fromIndex list)

            listWithoutItem =
                List.take fromIndex list ++ List.drop (fromIndex + 1) list
        in
            List.take toIndex listWithoutItem ++ listContainingItem ++ List.drop toIndex listWithoutItem


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

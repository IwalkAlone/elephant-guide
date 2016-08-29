module Components.Deck.Update exposing (..)

import Components.Deck.Model as Model exposing (..)
import Components.Archetype as Archetype
import Components.Card as Card
import DomManipulation
import ID exposing (..)
import Dict
import Ports
import Mouse
import Task exposing (Task)
import Json.Encode as JE
import Json.Decode as JD
import Http
import TableMetrics exposing (..)
import Slot exposing (..)
import Material exposing (init, subscriptions)


type DecklistKind
    = Maindeck
    | Sideboard


type Msg
    = AddArchetype
    | DeleteArchetype ID
    | AddCard
    | EditCardName ID String
    | CommitCardName
    | DeleteCard ID
    | EditSlot DecklistKind ID Int
    | SelectTab Int
    | DragStart Int
    | DragMove Mouse.Position
    | DragEnd Mouse.Position
    | ReceivedTableMetrics TableMetrics
    | SelectText String
    | EditArchetypeSlot Slot String
    | CommitArchetypeSlot
    | Hover (Maybe ID)
    | SetAddCardValue String
    | ArchetypeMsg ID Archetype.Msg
    | Mdl (Material.Msg Msg)
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( initialModel, Material.init Mdl )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddArchetype ->
            withSave
                { model
                    | archetypes = model.archetypes ++ [ { id = model.nextId, name = "New Archetype", weight = 0, decklist = Dict.empty, differenceOnTheDraw = Dict.empty } ]
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
                    | cards = model.cards ++ [ Card.Model model.nextId model.addCardValue ]
                    , addCardValue = ""
                    , nextId = model.nextId + 1
                }

        EditCardName cardId input ->
            { model | editState = EditingCardName cardId input } ! [ Ports.focus (DomManipulation.targetId (DomManipulation.CardNameInput cardId)) ]

        CommitCardName ->
            case model.editState of
                EditingCardName cardId value ->
                    let
                        updateCard card =
                            if card.id == cardId then
                                { card | name = value }
                            else
                                card
                    in
                        withSave
                            { model | cards = List.map updateCard model.cards, editState = NotEditing }

                _ ->
                    model ! []

        DeleteCard cardId ->
            withSave
                { model
                    | cards = List.filter (\card -> card.id /= cardId) model.cards
                    , maindeck = Dict.remove cardId model.maindeck
                    , sideboard = Dict.remove cardId model.sideboard
                    , archetypes = List.map (Archetype.deleteCard cardId) model.archetypes
                }

        EditSlot decklistKind cardId newValue ->
            let
                newModel =
                    case decklistKind of
                        Maindeck ->
                            { model | maindeck = Dict.insert cardId newValue model.maindeck }

                        Sideboard ->
                            { model | sideboard = Dict.insert cardId newValue model.sideboard }
            in
                withSave newModel

        SelectTab index ->
            { model | tab = index } ! []

        ArchetypeMsg id msg ->
            let
                updateArchetype archetype =
                    if archetype.id == id then
                        Archetype.update msg archetype
                    else
                        archetype
            in
                withSave { model | archetypes = List.map updateArchetype model.archetypes }

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

        SelectText elementId ->
            model ! [ Ports.selectText elementId ]

        EditArchetypeSlot slot input ->
            { model | editState = EditingArchetypeSlot slot input } ! []

        CommitArchetypeSlot ->
            case model.editState of
                EditingArchetypeSlot { cardId, archetypeId } value ->
                    let
                        updateArchetype archetype =
                            if archetype.id == archetypeId then
                                Archetype.setCardCounts archetype cardId (Slot.parsePlayDraw value)
                            else
                                archetype
                    in
                        withSave { model | editState = NotEditing, archetypes = List.map updateArchetype model.archetypes }

                _ ->
                    model ! []

        Hover maybeArchetypeId ->
            { model | hoverColumn = maybeArchetypeId } ! []

        SetAddCardValue value ->
            { model | addCardValue = value } ! []

        Mdl materialMsg ->
            Material.update materialMsg model

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
    Cmd.batch [ Task.perform (always NoOp) (always NoOp) (postDeck model), Ports.saveDeck (Model.encoder model) ]


postDeck : Model -> Task Http.Error String
postDeck model =
    Http.post JD.string "http://localhost:3000/save" (Model.encoder model |> (JE.encode 4) |> Http.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdl model
        , case model.tableMetrics of
            Nothing ->
                Ports.receiveTableMetrics ReceivedTableMetrics

            Just _ ->
                Sub.batch ([ Mouse.moves DragMove, Mouse.ups DragEnd ])
        ]

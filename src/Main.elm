module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import Dict exposing (..)
import String exposing (toInt)
import Components.Archetype as Archetype exposing (..)
import Components.Card as Card exposing (..)
import Ports exposing (..)


main : Program Never
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }


type alias ID =
    Int


type alias Slot =
    ( String, Int )


type alias Model =
    { archetypes : List ( ID, Archetype.Model )
    , cards : List ( ID, Card.Model )
    , slots : Dict ( ID, ID ) Int
    , nextId : ID
    }


type Msg
    = LoadDeck SavedDeckModel
    | AddArchetype
    | CopyArchetype
    | DeleteArchetype ID
    | AddCard
    | EditCard
    | DeleteCard
    | EditSlot ( ID, ID ) Int
    | ArchetypeMsg ID Archetype.Msg
    | CardMsg ID Card.Msg


initialArchetypes : List ( ID, Archetype.Model )
initialArchetypes =
    [ ( 0, { name = "Infect", weight = 8 } ), ( 1, { name = "Affinity", weight = 5 } ), ( 2, { name = "Jund", weight = 7 } ) ]


initialCards : List ( ID, Card.Model )
initialCards =
    [ ( 3, { name = "Path to Exile" } ), ( 4, { name = "Mana Leak" } ), ( 5, { name = "Supreme Verdict" } ) ]


initialModel : Model
initialModel =
    { archetypes = initialArchetypes
    , cards = initialCards
    , slots = initialSlots initialArchetypes initialCards
    , nextId = 6
    }


initialSlots : List ( ID, Archetype.Model ) -> List ( ID, Card.Model ) -> Dict ( ID, ID ) Int
initialSlots archetypes cards =
    let
        archetypeIds =
            List.map fst archetypes

        cardIds =
            List.map fst cards

        archetypePairs archetypeId =
            List.foldr (\cardId listSoFar -> ( archetypeId, cardId ) :: listSoFar) [] cardIds

        pairs =
            List.foldr (\archetypeId listSoFar -> List.concat [ archetypePairs archetypeId, listSoFar ]) [] archetypeIds

        pairsWithCounts =
            List.map (\pair -> ( pair, 4 )) pairs
    in
        Dict.fromList pairsWithCounts


slotValue : Model -> ( ID, ID ) -> Int
slotValue model pair =
    case Dict.get pair model.slots of
        Nothing ->
            Debug.crash "Accessed dictionary out of bounds"

        Just value ->
            value


init : Never -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDeck savedDeck ->
            ( { archetypes = savedDeck.archetypes
              , cards = savedDeck.cards
              , slots = Dict.fromList savedDeck.slots
              , nextId = savedDeck.nextId
              }
            , Cmd.none
            )

        AddArchetype ->
            let
                newModel =
                    { model
                        | archetypes = model.archetypes ++ [ ( model.nextId, { name = "New Archetype", weight = 0 } ) ]
                        , slots = List.foldr (\cardId dict -> Dict.insert ( model.nextId, cardId ) 0 dict) model.slots (List.map fst model.cards)
                        , nextId = model.nextId + 1
                    }
            in
                ( newModel, saveDeck newModel )

        DeleteArchetype id ->
            let
                newModel =
                    { model
                        | archetypes = List.filter (\( archetypeId, archetype ) -> archetypeId /= id) model.archetypes
                        , slots = Dict.filter (\( archetypeId, _ ) _ -> archetypeId /= id) model.slots
                    }
            in
                ( newModel, saveDeck newModel )

        AddCard ->
            let
                newModel =
                    { model
                        | cards = model.cards ++ [ ( model.nextId, { name = "New Card" } ) ]
                        , slots = List.foldr (\archetypeId dict -> Dict.insert ( archetypeId, model.nextId ) 0 dict) model.slots (List.map fst model.archetypes)
                        , nextId = model.nextId + 1
                    }
            in
                ( newModel, saveDeck newModel )

        EditSlot slot newValue ->
            let
                newModel =
                    { model | slots = Dict.insert slot newValue model.slots }
            in
                ( newModel, saveDeck newModel )

        ArchetypeMsg id msg ->
            let
                updateArchetype ( archetypeId, archetype ) =
                    if archetypeId == id then
                        ( archetypeId, Archetype.update msg archetype )
                    else
                        ( archetypeId, archetype )

                newModel =
                    { model | archetypes = List.map updateArchetype model.archetypes }
            in
                ( newModel, saveDeck newModel )

        CardMsg id msg ->
            let
                updateCard ( cardId, card ) =
                    if cardId == id then
                        ( cardId, Card.update msg card )
                    else
                        ( cardId, card )

                newModel =
                    { model | cards = List.map updateCard model.cards }
            in
                ( newModel, saveDeck newModel )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    table []
        ((viewHeader model :: viewLines model) ++ [ viewAddCard ])


viewHeader : Model -> Html Msg
viewHeader model =
    tr [] (cell (text "") :: List.map viewArchetype model.archetypes ++ [ viewAddArchetype ])


viewLines : Model -> List (Html Msg)
viewLines model =
    List.map (viewLine model) model.cards


viewLine : Model -> ( ID, Card.Model ) -> Html Msg
viewLine model ( id, card ) =
    tr [] (viewCard ( id, card ) :: List.map (\( archetypeId, archetype ) -> cell (slotInput model ( archetypeId, id ))) model.archetypes)


viewCard : ( ID, Card.Model ) -> Html Msg
viewCard ( id, model ) =
    (td [ class "card-cell" ] [ Html.map (CardMsg id) (Card.view model) ])


viewAddArchetype : Html Msg
viewAddArchetype =
    cell (button [ onClick AddArchetype ] [ text "+ Add Archetype" ])


viewArchetype : ( ID, Archetype.Model ) -> Html Msg
viewArchetype ( id, model ) =
    (td [ class "archetype-cell" ] [ Html.map (ArchetypeMsg id) (Archetype.view model), div [ class "buttons-overlay" ] [ viewArchetypeButtons id ] ])


viewArchetypeButtons : ID -> Html Msg
viewArchetypeButtons id =
    button [ onClick (DeleteArchetype id) ] [ text "Delete" ]


viewAddCard : Html Msg
viewAddCard =
    tr [] [ cell (button [ onClick AddCard ] [ text "+ Add Card" ]) ]


cell : Html msg -> Html msg
cell html =
    td [] [ html ]


slotInput : Model -> ( ID, ID ) -> Html Msg
slotInput model ( archetypeId, cardId ) =
    input
        [ type' "number"
        , value (toString (slotValue model ( archetypeId, cardId )))
        , onInput (\input -> EditSlot ( archetypeId, cardId ) (Result.withDefault 0 (String.toInt input)))
        ]
        []


saveDeck : Model -> Cmd msg
saveDeck model =
    let
        saveDeckModel =
            { archetypes = model.archetypes
            , cards = model.cards
            , slots = Dict.toList model.slots
            , nextId = model.nextId
            }
    in
        Ports.saveDeck saveDeckModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.loadDeck LoadDeck

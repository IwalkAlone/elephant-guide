module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import Dict exposing (..)
import String exposing (toInt)
import Components.Archetype as Archetype exposing (..)
import Components.Card as Card exposing (..)
import Components.Decklist as Decklist exposing (..)
import Ports exposing (..)
import ToFixed exposing (..)


main : Program Never
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }


type alias ID =
    Int


type alias Slot =
    ( String, Int )


type alias Model =
    { archetypes : List Archetype.Model
    , cards : List Card.Model
    , maindeck : Decklist
    , sideboard : Decklist
    , nextId : ID
    }


type DecklistKind
    = ArchetypeList ID
    | Maindeck
    | Sideboard


type Msg
    = LoadDeck SavedDeckModel
    | AddArchetype
    | DeleteArchetype ID
    | AddCard
    | EditSlot DecklistKind ID Int
    | ArchetypeMsg ID Archetype.Msg
    | CardMsg ID Card.Msg


initialModel : Model
initialModel =
    { archetypes = []
    , cards = []
    , maindeck = Dict.empty
    , sideboard = Dict.empty
    , nextId = 6
    }



-- initialSlots : List ( ID, Archetype.Model ) -> List ( ID, Card.Model ) -> Dict ( ID, ID ) Int
-- initialSlots archetypes cards =
--     let
--         archetypeIds =
--             List.map fst archetypes
--
--         cardIds =
--             List.map fst cards
--
--         archetypePairs archetypeId =
--             List.foldr (\cardId listSoFar -> ( archetypeId, cardId ) :: listSoFar) [] cardIds
--
--         pairs =
--             List.foldr (\archetypeId listSoFar -> List.concat [ archetypePairs archetypeId, listSoFar ]) [] archetypeIds
--
--         pairsWithCounts =
--             List.map (\pair -> ( pair, 4 )) pairs
--     in
--         Dict.fromList pairsWithCounts


init : Never -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDeck savedDeck ->
            let
                loadArchetype savedArchetype =
                    { id = savedArchetype.id
                    , name = savedArchetype.name
                    , weight = savedArchetype.weight
                    , decklist = Dict.fromList savedArchetype.decklist
                    }
            in
                ( { archetypes = List.map loadArchetype savedDeck.archetypes
                  , cards = savedDeck.cards
                  , maindeck = Dict.fromList savedDeck.maindeck
                  , sideboard = Dict.fromList savedDeck.sideboard
                  , nextId = savedDeck.nextId
                  }
                , Cmd.none
                )

        AddArchetype ->
            let
                newModel =
                    { model
                        | archetypes = model.archetypes ++ [ { id = model.nextId, name = "New Archetype", weight = 0, decklist = Dict.empty } ]
                        , nextId = model.nextId + 1
                    }
            in
                ( newModel, saveDeck newModel )

        DeleteArchetype id ->
            let
                newModel =
                    { model
                        | archetypes = List.filter (\archetype -> archetype.id /= id) model.archetypes
                    }
            in
                ( newModel, saveDeck newModel )

        AddCard ->
            let
                newModel =
                    { model
                        | cards = model.cards ++ [ { id = model.nextId, name = "New Card" } ]
                        , nextId = model.nextId + 1
                    }
            in
                ( newModel, saveDeck newModel )

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
                ( newModel, saveDeck newModel )

        ArchetypeMsg id msg ->
            let
                updateArchetype archetype =
                    if archetype.id == id then
                        Archetype.update msg archetype
                    else
                        archetype

                newModel =
                    { model | archetypes = List.map updateArchetype model.archetypes }
            in
                ( newModel, saveDeck newModel )

        CardMsg id msg ->
            let
                updateCard card =
                    if card.id == id then
                        Card.update msg card
                    else
                        card

                newModel =
                    { model | cards = List.map updateCard model.cards }
            in
                ( newModel, saveDeck newModel )


view : Model -> Html Msg
view model =
    table []
        ((viewHeader model :: viewLines model) ++ [ viewAddCard ])


viewHeader : Model -> Html Msg
viewHeader model =
    tr []
        (td [ class "card-cell" ] []
            :: viewDecklistHeader model.maindeck "Main" 60
            :: viewDecklistHeader model.sideboard "Side" 15
            :: List.map (viewArchetype model) model.archetypes
            ++ [ viewAddArchetype ]
        )


viewLines : Model -> List (Html Msg)
viewLines model =
    List.map (viewLine model) model.cards


viewLine : Model -> Card.Model -> Html Msg
viewLine model card =
    tr []
        (viewCard card
            :: viewMaindeckSideboard model card.id
            :: List.map (\archetype -> cell (slotInput (Decklist.slotValue archetype.decklist card.id) (EditSlot (ArchetypeList archetype.id) card.id)))
                model.archetypes
        )


viewMaindeckSideboard : Model -> ID -> Html Msg
viewMaindeckSideboard model cardId =
    td [ class "maindeck-sideboard-cell", colspan 2 ]
        [ slotInput (Decklist.slotValue model.maindeck cardId) (EditSlot Maindeck cardId)
        , div [ class "maindeck-sideboard-estimated" ]
            [ div [] [ text (recommendedMaindeckCountOfCard model cardId |> toFixed 2) ]
            , hr [] []
            , div [] [ text (toString (maxCountOfCard model cardId)) ]
            ]
        , slotInput (Decklist.slotValue model.sideboard cardId) (EditSlot Sideboard cardId)
        ]


recommendedMaindeckCountOfCard : Model -> ID -> Float
recommendedMaindeckCountOfCard model cardId =
    let
        weightedCounts =
            List.map (\archetype -> (toFloat (Decklist.slotValue archetype.decklist cardId) * archetype.weight)) model.archetypes

        totalWeight =
            model.archetypes
                |> List.map .weight
                |> List.sum
    in
        if totalWeight == 0 then
            0
        else
            (List.sum weightedCounts) / totalWeight


maxCountOfCard : Model -> ID -> Int
maxCountOfCard model cardId =
    let
        counts =
            List.map (\archetype -> Decklist.slotValue archetype.decklist cardId) model.archetypes
    in
        Maybe.withDefault 0 (List.maximum counts)


viewCard : Card.Model -> Html Msg
viewCard model =
    (td [ class "card-cell" ] [ Html.map (CardMsg model.id) (Card.view model) ])


viewAddArchetype : Html Msg
viewAddArchetype =
    cell (button [ onClick AddArchetype ] [ text "+ Add Archetype" ])


viewArchetype : Model -> Archetype.Model -> Html Msg
viewArchetype model archetype =
    (td [ class "archetype-cell" ]
        [ div [] [ Html.map (ArchetypeMsg archetype.id) (Archetype.viewName archetype) ]
        , div [] [ Html.map (ArchetypeMsg archetype.id) (Archetype.viewWeight archetype), viewCardCount (Decklist.cardCount archetype.decklist) 60 ]
        ]
    )


viewDecklistHeader : Decklist -> String -> Int -> Html Msg
viewDecklistHeader decklist name targetCount =
    (td [ class "archetype-cell" ]
        [ div [] [ text name ], div [] [ viewCardCount (Decklist.cardCount decklist) targetCount ] ]
    )


viewArchetypeButtons : ID -> Html Msg
viewArchetypeButtons id =
    button [ onClick (DeleteArchetype id) ] [ text "Delete" ]


viewCardCount : Int -> Int -> Html Msg
viewCardCount count targetCount =
    span [ classList [ ( "invalid-count", count /= targetCount ) ] ] [ text (toString count ++ "/" ++ toString targetCount) ]


viewAddCard : Html Msg
viewAddCard =
    tr [] [ cell (button [ onClick AddCard ] [ text "+ Add Card" ]) ]


cell : Html msg -> Html msg
cell html =
    td [] [ html ]


slotInput : Int -> (Int -> Msg) -> Html Msg
slotInput currentValue saveCountMsg =
    let
        viewValue =
            case currentValue of
                0 ->
                    ""

                value ->
                    toString value
    in
        input
            [ type' "number"
            , value viewValue
            , onInput (\input -> saveCountMsg (Result.withDefault 0 (String.toInt input)))
            ]
            []


saveDeck : Model -> Cmd msg
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
        Ports.saveDeck saveDeckModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.loadDeck LoadDeck

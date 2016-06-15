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
                loadArchetype ( id, savedArchetype ) =
                    ( id
                    , { name = savedArchetype.name
                      , weight = savedArchetype.weight
                      , decklist = Dict.fromList savedArchetype.decklist
                      }
                    )
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
                        | archetypes = model.archetypes ++ [ ( model.nextId, { name = "New Archetype", weight = 0, decklist = Dict.empty } ) ]
                        , nextId = model.nextId + 1
                    }
            in
                ( newModel, saveDeck newModel )

        DeleteArchetype id ->
            let
                newModel =
                    { model
                        | archetypes = List.filter (\( archetypeId, archetype ) -> archetypeId /= id) model.archetypes
                    }
            in
                ( newModel, saveDeck newModel )

        AddCard ->
            let
                newModel =
                    { model
                        | cards = model.cards ++ [ ( model.nextId, { name = "New Card" } ) ]
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
                                updateArchetype ( id, archetype ) =
                                    if archetypeId == id then
                                        ( id, { archetype | decklist = Dict.insert cardId newValue archetype.decklist } )
                                    else
                                        ( id, archetype )
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


view : Model -> Html Msg
view model =
    table []
        ((viewHeader model :: viewLines model) ++ [ viewAddCard ])


viewHeader : Model -> Html Msg
viewHeader model =
    tr [] (td [ class "card-cell" ] [] :: td [ class "archetype-cell" ] [ text "Main" ] :: td [ class "archetype-cell" ] [ text "Side" ] :: List.map (viewArchetype model) model.archetypes ++ [ viewAddArchetype ])


viewLines : Model -> List (Html Msg)
viewLines model =
    List.map (viewLine model) model.cards


viewLine : Model -> ( ID, Card.Model ) -> Html Msg
viewLine model ( id, card ) =
    tr []
        (viewCard ( id, card )
            :: cell (slotInput (Decklist.slotValue model.maindeck id) (EditSlot Maindeck id))
            :: cell (slotInput (Decklist.slotValue model.sideboard id) (EditSlot Sideboard id))
            :: List.map (\( archetypeId, archetype ) -> cell (slotInput (Decklist.slotValue archetype.decklist id) (EditSlot (ArchetypeList archetypeId) id)))
                model.archetypes
        )


viewCard : ( ID, Card.Model ) -> Html Msg
viewCard ( id, model ) =
    (td [ class "card-cell" ] [ Html.map (CardMsg id) (Card.view model) ])


viewDecklistSlot list ( id, model ) =
    []


viewAddArchetype : Html Msg
viewAddArchetype =
    cell (button [ onClick AddArchetype ] [ text "+ Add Archetype" ])


viewArchetype : Model -> ( ID, Archetype.Model ) -> Html Msg
viewArchetype model ( id, archetype ) =
    (td [ class "archetype-cell" ]
        [ div [] [ Html.map (ArchetypeMsg id) (Archetype.viewName archetype) ]
        , div [] [ Html.map (ArchetypeMsg id) (Archetype.viewWeight archetype), viewCardCount (Decklist.cardCount archetype.decklist) ]
        ]
    )


viewArchetypeButtons : ID -> Html Msg
viewArchetypeButtons id =
    button [ onClick (DeleteArchetype id) ] [ text "Delete" ]


viewCardCount : Int -> Html Msg
viewCardCount count =
    span [ classList [ ( "invalid-count", count /= 60 ) ] ] [ text (toString count ++ "/60") ]


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
        saveArchetype ( id, archetype ) =
            ( id
            , { weight = archetype.weight
              , name = archetype.name
              , decklist = Dict.toList archetype.decklist
              }
            )

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

module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Dict exposing (..)
import Components.Deck.Model as Deck exposing (..)
import Components.Deck.Update as Deck exposing (..)
import Components.Deck.View as Deck exposing (..)
import Http
import Json.Decode as JD exposing (string)
import Json.Encode as JE
import Task exposing (Task)
import Ports exposing (..)


main : Program Never
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }


type alias ID =
    Int


type alias Model =
    { deck : Deck.Model
    }


type Msg
    = LoadDeck SavedDeckModel
    | DeckMsg Deck.Msg
    | NoOp


initialModel : Model
initialModel =
    { deck = Deck.initialModel
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

                loadedDeck =
                    { archetypes = List.map loadArchetype savedDeck.archetypes
                    , cards = savedDeck.cards
                    , maindeck = Dict.fromList savedDeck.maindeck
                    , sideboard = Dict.fromList savedDeck.sideboard
                    , nextId = savedDeck.nextId
                    }
            in
                { model | deck = loadedDeck } ! []

        DeckMsg msg ->
            let
                newDeck =
                    Deck.update msg model.deck
            in
                { model | deck = newDeck } ! [ saveDeck newDeck ]

        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    Html.map DeckMsg (Deck.view model.deck)


saveDeck : Deck.Model -> Cmd Msg
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


postDeck : Deck.Model -> Task Http.Error String
postDeck model =
    Http.post string "http://localhost:3000/save" (Deck.encoder model |> (JE.encode 4) |> Http.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.loadDeck LoadDeck

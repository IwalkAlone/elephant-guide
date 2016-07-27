module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Lazy exposing (..)
import Dict exposing (..)
import Components.Deck.Model as Deck exposing (..)
import Components.Deck.Update as Deck exposing (..)
import Components.Deck.View as Deck exposing (..)
import Http
import Task exposing (Task)
import Ports exposing (..)


main : Program Never
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { deck : Deck.Model
    }


type Msg
    = LoadDeck SavedDeckModel
    | GetDeck Deck.Model
    | GetDeckError Http.Error
    | DeckMsg Deck.Msg
    | NoOp


initialModel : Model
initialModel =
    { deck = Deck.initialModel
    }


init : Never -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Task.perform GetDeckError GetDeck getDeck )


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
                    , cardIndexBeingDragged = Nothing
                    , tableMetrics = Nothing
                    , dragInsertAtIndex =
                        Nothing
                        --    , dragState = NotDragging
                    }
            in
                { model | deck = loadedDeck } ! []

        GetDeck deck ->
            { model | deck = deck } ! []

        GetDeckError error ->
            let
                debug =
                    Debug.log "Error loading deck: " error
            in
                model ! []

        DeckMsg msg ->
            let
                ( newDeck, cmd ) =
                    Deck.update msg model.deck
            in
                { model | deck = newDeck } ! [ Cmd.map DeckMsg cmd ]

        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    Html.map DeckMsg (lazy Deck.view model.deck)


getDeck : Task Http.Error Deck.Model
getDeck =
    Http.get Deck.decoder "http://localhost:3000/deck"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DeckMsg (Deck.subscriptions model.deck)

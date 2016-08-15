module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Lazy exposing (..)
import Components.Deck.Model as Deck exposing (..)
import Components.Deck.Update as Deck exposing (..)
import Components.Deck.View as Deck exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import Task exposing (Task)
import Ports
import Material.Layout


main : Program Never
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { deck : Deck.Model
    }


type Msg
    = LoadDeckFromLocalDb JE.Value
    | GetDeckFromServer Deck.Model
    | GetDeckError Http.Error
    | DeckMsg Deck.Msg
    | NoOp


init : Never -> ( Model, Cmd Msg )
init flags =
    let
        ( deck, cmd ) =
            Deck.init
    in
        { deck = { deck | mdl = Material.Layout.setTabsWidth 300 deck.mdl } }
            ! [ Cmd.map DeckMsg cmd, Task.perform GetDeckError GetDeckFromServer getDeck ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDeckFromLocalDb deck ->
            let
                deckResult =
                    decodeValue Deck.decoder deck
            in
                case deckResult of
                    Ok loadedDeck ->
                        { model | deck = loadedDeck } ! []

                    Err error ->
                        model ! [ Task.perform GetDeckError GetDeckFromServer getDeck ]

        GetDeckFromServer deck ->
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
    Sub.batch [ Ports.loadDeck LoadDeckFromLocalDb, Sub.map DeckMsg (Deck.subscriptions model.deck) ]

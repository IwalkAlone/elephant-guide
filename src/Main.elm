module Main exposing (..)

import Html exposing (..)
import Html.Lazy exposing (..)
import Components.Deck.Model as Deck exposing (..)
import Components.Deck.Update as Deck exposing (..)
import Components.Deck.View as Deck exposing (..)
import Http
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import Ports


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { deck : Deck.Model
    }


type Msg
    = LoadDeckFromLocalDb JE.Value
    | GotDeck (Result Http.Error Deck.Model)
    | DeckMsg Deck.Msg
    | NoOp


init : ( Model, Cmd Msg )
init =
    let
        ( deck, cmd ) =
            Deck.init
    in
        { deck = deck }
            ! [ Cmd.map DeckMsg cmd, Http.send GotDeck getDeck ]


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
                        model ! [ Http.send GotDeck getDeck ]

        GotDeck (Ok deck) ->
            let
                ( _, cmd ) =
                    Deck.init
            in
                { model | deck = deck } ! [ Cmd.map DeckMsg cmd ]

        GotDeck (Err error) ->
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
    Html.map DeckMsg (Html.Lazy.lazy Deck.view model.deck)


getDeck : Http.Request Deck.Model
getDeck =
    Http.get "http://localhost:3000/deck" Deck.decoder


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Ports.loadDeck LoadDeckFromLocalDb, Sub.map DeckMsg (Deck.subscriptions model.deck) ]

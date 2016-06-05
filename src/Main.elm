module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import Dict exposing (..)


main : Program Never
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }


type alias ID =
    Int


type alias Slot =
    ( String, Int )


type alias Archetype =
    { name : String
    }


type alias Card =
    { name : String
    }


type alias Model =
    { archetypes : List ( ID, Archetype )
    , cards : List ( ID, Card )
    , slots : Dict ( ID, ID ) Int
    , nextId : ID
    }


initialArchetypes : List ( ID, Archetype )
initialArchetypes =
    [ ( 0, { name = "Infect" } ), ( 1, { name = "Affinity" } ), ( 2, { name = "Jund" } ) ]


initialCards : List ( ID, Card )
initialCards =
    [ ( 3, { name = "Path to Exile" } ), ( 4, { name = "Mana Leak" } ), ( 5, { name = "Supreme Verdict" } ) ]


initialModel : Model
initialModel =
    { archetypes = initialArchetypes
    , cards = initialCards
    , slots = initialSlots initialArchetypes initialCards
    , nextId = 6
    }


initialSlots : List ( ID, Archetype ) -> List ( ID, Card ) -> Dict ( ID, ID ) Int
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


type Msg
    = AddArchetype
    | EditArchetype
    | CopyArchetype
    | DeleteArchetype
    | AddCard
    | EditCard
    | DeleteCard
    | EditSlot


init : Never -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text (toString (model.nextId + 1))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

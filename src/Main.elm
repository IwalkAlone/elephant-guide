module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import Dict exposing (..)
import String exposing (toInt)
import Components.Archetype as Archetype exposing (..)


main : Program Never
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }


type alias ID =
    Int


type alias Slot =
    ( String, Int )


type alias Card =
    { name : String
    }


type alias Model =
    { archetypes : List ( ID, Archetype.Model )
    , cards : List ( ID, Card )
    , slots : Dict ( ID, ID ) Int
    , nextId : ID
    }


initialArchetypes : List ( ID, Archetype.Model )
initialArchetypes =
    [ ( 0, { name = "Infect", weight = 8 } ), ( 1, { name = "Affinity", weight = 5 } ), ( 2, { name = "Jund", weight = 7 } ) ]


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


initialSlots : List ( ID, Archetype.Model ) -> List ( ID, Card ) -> Dict ( ID, ID ) Int
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


type Msg
    = AddArchetype
    | CopyArchetype
    | DeleteArchetype
    | AddCard
    | EditCard
    | DeleteCard
    | EditSlot ( ID, ID ) Int
    | ArchetypeMsg ID Archetype.Msg


init : Never -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddArchetype ->
            ( { model
                | archetypes = model.archetypes ++ [ ( model.nextId, { name = "New Archetype", weight = 0 } ) ]
                , slots = List.foldr (\cardId dict -> Dict.insert ( model.nextId, cardId ) 0 dict) model.slots (List.map fst model.cards)
                , nextId = model.nextId + 1
              }
            , Cmd.none
            )

        AddCard ->
            ( { model
                | cards = model.cards ++ [ ( model.nextId, { name = "New Card" } ) ]
                , slots = List.foldr (\archetypeId dict -> Dict.insert ( archetypeId, model.nextId ) 0 dict) model.slots (List.map fst model.archetypes)
                , nextId = model.nextId + 1
              }
            , Cmd.none
            )

        EditSlot slot newValue ->
            ( { model | slots = Dict.insert slot newValue model.slots }, Cmd.none )

        ArchetypeMsg id msg ->
            let
                updateArchetype ( archetypeId, archetype ) =
                    if archetypeId == id then
                        ( archetypeId, Archetype.update msg archetype )
                    else
                        ( archetypeId, archetype )
            in
                ( { model | archetypes = List.map updateArchetype model.archetypes }, Cmd.none )

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


viewLine : Model -> ( ID, Card ) -> Html Msg
viewLine model ( id, card ) =
    tr [] (cell (text card.name) :: List.map (\( archetypeId, archetype ) -> cell (slotInput model ( archetypeId, id ))) model.archetypes)


viewAddArchetype : Html Msg
viewAddArchetype =
    cell (button [ onClick AddArchetype ] [ text "+ Add Archetype" ])


viewArchetype : ( ID, Archetype.Model ) -> Html Msg
viewArchetype ( id, model ) =
    Html.map (ArchetypeMsg id) (cell (Archetype.view model))


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

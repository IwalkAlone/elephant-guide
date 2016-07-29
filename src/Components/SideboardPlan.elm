module Components.SideboardPlan exposing (..)

import Components.Deck.Model as Deck exposing (..)
import Components.Archetype as Archetype exposing (..)
import Components.Decklist as Decklist exposing (..)
import ID exposing (ID)
import Html exposing (..)
import Dict
import String


viewSideboardPlans : Deck.Model -> Archetype.Model -> Html msg
viewSideboardPlans deck archetype =
    let
        a =
            1
    in
        if Archetype.hasDifferencesOnTheDraw archetype then
            div [] []
        else
            div [] []


viewSideboardPlan : String -> List ( String, Int ) -> Html msg
viewSideboardPlan name exchanges =
    div []
        [ h1 [] [ text name ]
        , ul [] (List.map (\item -> li [] [ text (displaySideboardPlanItem item) ]) exchanges)
        ]


sideboardPlan : Deck.Model -> Decklist -> List ( String, Int )
sideboardPlan deck matchupList =
    Dict.merge
        (\cardId maindeckCount currentPlan -> List.append currentPlan [ ( getCardNameById deck cardId, -maindeckCount ) ])
        (\cardId maindeckCount matchupCount currentPlan -> currentPlan)
        (\cardId matchupCount currentPlan -> List.append currentPlan [ ( getCardNameById deck cardId, matchupCount ) ])
        deck.maindeck
        matchupList
        []


displaySideboardPlanItem : ( String, Int ) -> String
displaySideboardPlanItem ( name, diff ) =
    if diff > 0 then
        String.join " " [ "+", toString diff, name ]
    else if diff < 0 then
        String.join " " [ "-", (toString << abs) diff, name ]
    else
        ""


getCardNameById : Deck.Model -> ID -> String
getCardNameById model cardId =
    let
        maybeCard =
            model.cards
                |> List.filter (\card -> card.id == cardId)
                |> List.head
    in
        case maybeCard of
            Nothing ->
                Debug.crash "Card not found with ID: " ++ toString cardId

            Just card ->
                card.name

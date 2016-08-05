module Components.SideboardPlan exposing (..)

import Components.Deck.Model as Deck exposing (..)
import Components.Archetype as Archetype exposing (..)
import Components.Decklist as Decklist exposing (..)
import Html exposing (..)
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
    let
        countById id =
            Decklist.slotValue matchupList id - Decklist.slotValue deck.maindeck id
    in
        deck.cards
            |> List.map (\card -> ( card.name, countById card.id ))
            |> List.filter (\( cardName, qty ) -> qty /= 0)
            |> List.sortBy snd


displaySideboardPlanItem : ( String, Int ) -> String
displaySideboardPlanItem ( name, diff ) =
    if diff > 0 then
        String.join " " [ "+", toString diff, name ]
    else if diff < 0 then
        String.join " " [ "-", (toString << abs) diff, name ]
    else
        ""

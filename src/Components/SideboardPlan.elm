module Components.SideboardPlan exposing (..)

import Components.Deck.Model as Deck exposing (..)
import Components.Archetype as Archetype exposing (..)
import Components.Decklist as Decklist exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id, class, href)
import Material.Card as Card
import Material.Elevation as Elevation
import String


viewSideboardPlans : Deck.Model -> Archetype.Model -> Html msg
viewSideboardPlans deck archetype =
    let
        planOnThePlay =
            viewSideboardPlan (sideboardPlan deck archetype.decklist)

        materialCard content =
            Card.view
                [ Elevation.e4 ]
                [ Card.title [ Card.border ] [ text archetype.name ]
                , Card.text [] [ content ]
                ]
    in
        if Archetype.hasDifferencesOnTheDraw archetype then
            let
                planOnTheDraw =
                    viewSideboardPlan (sideboardPlan deck (Archetype.decklistOnTheDraw archetype))
            in
                materialCard
                    (div [ class "separate-sideboard-plans-for-play-draw" ]
                        [ div [] [ span [] [ text "On the play:" ], planOnThePlay ]
                        , div [] [ span [] [ text "On the draw:" ], planOnTheDraw ]
                        ]
                    )
        else
            materialCard planOnThePlay


viewSideboardPlan : List ( String, Int ) -> Html msg
viewSideboardPlan exchanges =
    ul [] (List.map (\item -> li [] [ text (displaySideboardPlanItem item) ]) exchanges)


sideboardPlan : Deck.Model -> Decklist -> List ( String, Int )
sideboardPlan deck matchupList =
    let
        countById id =
            Decklist.slotValue matchupList id - Decklist.slotValue deck.maindeck id
    in
        deck.cards
            |> List.map (\card -> ( card.name, countById card.id ))
            |> List.filter (\( cardName, qty ) -> qty /= 0)
            |> List.sortBy Tuple.second


displaySideboardPlanItem : ( String, Int ) -> String
displaySideboardPlanItem ( name, diff ) =
    if diff > 0 then
        String.join " " [ "+", toString diff, name ]
    else if diff < 0 then
        String.join " " [ "-", (toString << abs) diff, name ]
    else
        ""

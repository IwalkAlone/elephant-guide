module Components.Deck.View exposing (..)

import Components.Deck.Model as Model exposing (..)
import Components.Deck.Update as Update exposing (..)
import Components.Archetype as Archetype
import Components.Card as Card
import Components.Decklist as Decklist exposing (..)
import Components.SideboardPlan exposing (..)
import ID exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed exposing (..)
import String
import ToFixed exposing (toFixed)
import DomManipulation exposing (..)
import Slot exposing (..)
import Material.Layout as Layout exposing (render, row, title)
import Material.Tabs as Tabs exposing (render, ripple, onSelectTab, activeTab, textLabel)


--import Material.Table as Table exposing (table, thead, tbody, tr, th, td)


view : Model -> Html Msg
view model =
    let
        elephant =
            div []
                [ keyedTable [] ([ ( "$Header", viewHeader model ) ] ++ viewLines model ++ [ ( "$AddCard", viewAddCard ) ]) ]

        sideboardPlans =
            List.map (\archetype -> viewSideboardPlans model archetype) model.archetypes
    in
        Layout.render Mdl
            model.mdl
            []
            { header =
                [ Layout.row []
                    [ Layout.title [] [ text "Elephant Guide" ]
                    ]
                ]
            , drawer =
                [ Layout.title [] [ text "Menu" ]
                , Layout.navigation []
                    [ Layout.link [] [ a [] [ text "New Deck" ] ]
                    , hr [] []
                    , Layout.link [] [ a [] [ text "Faeries" ] ]
                    , Layout.link [] [ a [] [ text "BG Delirium" ] ]
                    ]
                ]
            , tabs = ( [], [] )
            , main =
                [ Tabs.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Tabs.ripple
                    , Tabs.onSelectTab SelectTab
                    , Tabs.activeTab model.tab
                    ]
                    [ Tabs.textLabel [] "Elephant"
                    , Tabs.textLabel [] "Plan preview"
                    ]
                    (case model.tab of
                        0 ->
                            [ elephant ]

                        _ ->
                            sideboardPlans
                    )
                ]
            }


viewHeader : Model -> Html Msg
viewHeader model =
    thead []
        (td [ class "card-cell" ] [ text ("Total Slots Used: " ++ toString (totalUsedSlots model)) ]
            :: viewDecklistHeader model.maindeck "Main" 60
            :: viewDecklistHeader model.sideboard "Side" 15
            :: List.map (viewArchetype model) model.archetypes
            ++ [ viewAddArchetype ]
        )


viewLines : Model -> List ( String, Html Msg )
viewLines model =
    List.indexedMap (viewLine model) model.cards


viewLine : Model -> Int -> Card.Model -> ( String, Html Msg )
viewLine model index card =
    let
        classes =
            case model.dragState of
                NotDragging ->
                    []

                Dragging fromIndex toIndex ->
                    let
                        displayedToIndex =
                            if fromIndex + 1 == toIndex then
                                toIndex + 1
                            else
                                toIndex
                    in
                        [ ( "drop-target-above", displayedToIndex == index )
                        , ( "drop-target-below", displayedToIndex == index + 1 )
                        , ( "dragging", fromIndex == index )
                        ]
    in
        ( "$Card" ++ card.name
        , tr
            [ classList classes ]
            (viewCard card index
                :: viewMaindeckSideboard model card.id
                :: List.map
                    (\archetype ->
                        cell
                            (archetypeSlotInput
                                (Slot card.id archetype.id)
                                (currentSlotDisplayValue model archetype card.id)
                                (Archetype.slotDisplayValue archetype card.id)
                                (targetId
                                    (MatchupInput (Slot card.id archetype.id))
                                )
                            )
                    )
                    model.archetypes
            )
        )


currentSlotDisplayValue : Model -> Archetype.Model -> ID -> String
currentSlotDisplayValue model archetype cardId =
    let
        displayValue =
            Archetype.slotDisplayValue archetype cardId
    in
        case model.slotEdit of
            Nothing ->
                displayValue

            Just { slot, value } ->
                if slot == Slot cardId archetype.id then
                    value
                else
                    displayValue


viewMaindeckSideboard : Model -> ID -> Html Msg
viewMaindeckSideboard model cardId =
    td
        [ classList
            [ ( "maindeck-sideboard-cell", True )
            , ( "maindeck-sideboard-cell-invalid", not (slotsFulfilledByMaindeckSideboard model cardId) )
            ]
        , colspan 2
        ]
        [ slotInput (Decklist.slotValue model.maindeck cardId) (EditSlot Maindeck cardId) (targetId (MaindeckInput cardId))
        , div [ class "maindeck-sideboard-estimated" ]
            [ div [] [ text (recommendedMaindeckCountOfCard model cardId |> toFixed 2) ]
            , hr [] []
            , div [] [ text (toString (maxCountOfCard model cardId)) ]
            ]
        , slotInput (Decklist.slotValue model.sideboard cardId) (EditSlot Sideboard cardId) (targetId (SideboardInput cardId))
        ]


slotsFulfilledByMaindeckSideboard : Model -> ID -> Bool
slotsFulfilledByMaindeckSideboard model cardId =
    Decklist.slotValue model.maindeck cardId + Decklist.slotValue model.sideboard cardId == maxCountOfCard model cardId


recommendedMaindeckCountOfCard : Model -> ID -> Float
recommendedMaindeckCountOfCard model cardId =
    let
        weightedCounts =
            List.map (Archetype.weightedCardCount cardId) model.archetypes

        totalWeight =
            model.archetypes
                |> List.map .weight
                |> List.sum
    in
        if totalWeight == 0 then
            0
        else
            (List.sum weightedCounts) / totalWeight


totalUsedSlots : Model -> Int
totalUsedSlots model =
    model.cards
        |> List.map .id
        |> List.map (maxCountOfCard model)
        |> List.sum


maxCountOfCard : Model -> ID -> Int
maxCountOfCard model cardId =
    let
        counts =
            List.map (Archetype.maxCardCount cardId) model.archetypes
    in
        Maybe.withDefault 0 (List.maximum counts)


viewCard : Card.Model -> Int -> Html Msg
viewCard model index =
    (td [ class "card-cell", onMouseDown (DragStart index) ] [ Html.map (CardMsg model.id) (Card.view model), div [ onClick (DeleteCard model.id) ] [ text "Del" ] ])


viewAddArchetype : Html Msg
viewAddArchetype =
    cell (button [ onClick AddArchetype ] [ text "+ Add Archetype" ])


viewArchetype : Model -> Archetype.Model -> Html Msg
viewArchetype model archetype =
    let
        cardCountElements =
            List.map (\decklist -> viewCardCount (Decklist.cardCount decklist) 60) (Archetype.decklists archetype)
    in
        (td [ class "archetype-cell" ]
            [ div [] [ Html.map (ArchetypeMsg archetype.id) (Archetype.viewName archetype) ]
            , div [] ((Html.map (ArchetypeMsg archetype.id) (Archetype.viewWeight archetype)) :: cardCountElements)
            , div [] [ a [ href ("#" ++ targetId (ArchetypeSideboardPlanAnchor archetype.id)) ] [ text "Preview" ] ]
            ]
        )


viewDecklistHeader : Decklist -> String -> Int -> Html Msg
viewDecklistHeader decklist name targetCount =
    (td [ class "archetype-cell" ]
        [ div [] [ text name ], div [] [ viewCardCount (Decklist.cardCount decklist) targetCount ] ]
    )


viewArchetypeButtons : ID -> Html Msg
viewArchetypeButtons id =
    button [ onClick (DeleteArchetype id) ] [ text "Delete" ]


viewCardCount : Int -> Int -> Html Msg
viewCardCount count targetCount =
    span [ classList [ ( "count-over", count > targetCount ), ( "count-under", count < targetCount ) ] ] [ text (toString count ++ "/" ++ toString targetCount) ]


viewAddCard : Html Msg
viewAddCard =
    tr [] [ cell (button [ onClick AddCard ] [ text "+ Add Card" ]) ]


cell : Html msg -> Html msg
cell html =
    td [] [ html ]


slotInput : Int -> (Int -> Msg) -> String -> Html Msg
slotInput currentValue saveCountMsg elementId =
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
            , onClick (FocusAndSelect elementId)
            , id elementId
            ]
            []


archetypeSlotInput : Slot -> String -> String -> String -> Html Msg
archetypeSlotInput slot currentValue initialEditValue elementId =
    input
        [ type' "text"
        , onFocus (ArchetypeSlotStartEditing (SlotEdit slot initialEditValue))
        , onInput (ArchetypeSlotInput slot)
        , onBlur (ArchetypeSlotFinishEditing slot)
        , onClick (FocusAndSelect elementId)
        , value currentValue
        , id elementId
        ]
        []


keyedTable : List (Attribute a) -> List ( String, Html a ) -> Html a
keyedTable =
    Html.Keyed.node "table"

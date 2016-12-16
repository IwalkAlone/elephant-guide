module Components.Deck.View exposing (..)

import Components.Deck.Model as Model exposing (..)
import Components.Deck.Update as Update exposing (..)
import Components.Archetype as Archetype
import Components.Card as Card
import Components.Decklist as Decklist exposing (..)
import Components.DeckSettings as DeckSettings
import Components.SideboardPlan exposing (..)
import ID exposing (..)
import Json.Decode as JD
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed exposing (..)
import String
import ToFixed exposing (toFixed)
import DomManipulation exposing (..)
import Slot exposing (..)
import Material.Layout as Layout
import Material.Button as Button
import Material.Icon as Icon


view : Model -> Html Msg
view model =
    let
        elephant =
            div []
                [ keyedTable [] ([ ( "$Header", viewHeader model ) ] ++ viewLines model ++ [ ( "$AddCard", viewAddCard model.addCardValue ) ]) ]

        sideboardPlans =
            List.map (\archetype -> viewSideboardPlans model archetype) model.archetypes
    in
        Layout.render Mdl
            model.mdl
            [ Layout.selectedTab model.tab
            , Layout.onSelectTab SelectTab
            ]
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
            , tabs = ( [ text "Elephant", text "Plan preview", text "Settings" ], [] )
            , main =
                case model.tab of
                    0 ->
                        [ elephant ]

                    1 ->
                        [ div [ class "sideboard-plan-grid" ] sideboardPlans ]

                    _ ->
                        [ DeckSettings.view model ]
            }


viewHeader : Model -> Html Msg
viewHeader model =
    thead []
        (td [ class "card-cell" ] [ text ("Total Slots Used: " ++ toString (totalUsedSlots model)) ]
            :: viewDecklistHeader model.maindeck "Main" model.targetSize
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
            ([ viewCard model card index ]
                ++ viewMaindeckSideboard model card.id
                ++ List.map
                    (\archetype ->
                        td
                            [ onMouseEnter (Hover (Just archetype.id))
                            , onMouseLeave (Hover Nothing)
                            , classList [ ( "highlighted", model.hoverColumn == Just archetype.id ) ]
                            ]
                            [ archetypeSlotInput
                                (Slot card.id archetype.id)
                                (currentSlotDisplayValue model archetype card.id)
                                (Archetype.slotDisplayValue archetype card.id)
                                (targetId
                                    (MatchupInput (Slot card.id archetype.id))
                                )
                            ]
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
        case model.editState of
            EditingArchetypeSlot slot value ->
                if slot == Slot cardId archetype.id then
                    value
                else
                    displayValue

            _ ->
                displayValue


viewMaindeckSideboard : Model -> ID -> List (Html Msg)
viewMaindeckSideboard model cardId =
    let
        cell contents =
            td
                [ classList
                    [ ( "maindeck-sideboard-cell", True )
                    , ( "maindeck-sideboard-cell-invalid", not (slotsFulfilledByMaindeckSideboard model cardId) )
                    ]
                ]
                contents
    in
        [ cell
            [ slotInput (Decklist.slotValue model.maindeck cardId) (EditSlot Maindeck cardId) (targetId (MaindeckInput cardId)) ]
        , cell
            [ slotInput (Decklist.slotValue model.sideboard cardId) (EditSlot Sideboard cardId) (targetId (SideboardInput cardId)) ]
        ]



-- , div [ class "maindeck-sideboard-estimated" ]
--     [ div [] [ text (recommendedMaindeckCountOfCard model cardId |> toFixed 2) ]
--     , hr [] []
--     , div [] [ text (toString (maxCountOfCard model cardId)) ]
--     ]


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


viewCard : Model -> Card.Model -> Int -> Html Msg
viewCard model card index =
    let
        editingThisCard =
            case model.editState of
                EditingCardName cardId value ->
                    cardId == card.id

                _ ->
                    False

        cardNameInput =
            input [ class "card-name", type_ "text", defaultValue card.name, id (targetId (CardNameInput card.id)), onInput (EditCardName card.id), onBlur CommitCardName ] []

        cardNameSpan =
            span [ class "card-name" ] [ text card.name ]
    in
        (td
            (class "card-cell"
                :: (if not editingThisCard then
                        [ onMouseDown (DragStart index) ]
                    else
                        []
                   )
            )
            [ Button.render Mdl [ 1, card.id ] model.mdl [ Button.icon, Button.onClick (DeleteCard card.id) ] [ Icon.view "delete" [ Icon.size18 ] ]
            , Button.render Mdl [ 2, card.id ] model.mdl [ Button.icon, Button.onClick (EditCardName card.id card.name) ] [ Icon.view "create" [ Icon.size18 ] ]
            , div [ class "card" ]
                [ if editingThisCard then
                    cardNameInput
                  else
                    cardNameSpan
                ]
            ]
        )


viewAddArchetype : Html Msg
viewAddArchetype =
    td [] [ button [ onClick AddArchetype ] [ text "+ Add Archetype" ] ]


viewArchetype : Model -> Archetype.Model -> Html Msg
viewArchetype model archetype =
    let
        cardCountElements =
            List.map (\decklist -> viewCardCount (Decklist.cardCount decklist) model.targetSize) (Archetype.decklists archetype)
    in
        (td [ classList [ ( "archetype-cell", True ), ( "highlighted", model.hoverColumn == Just archetype.id ) ] ]
            [ div [] [ Html.map (ArchetypeMsg archetype.id) (Archetype.viewName archetype) ]
            , div [] ((Html.map (ArchetypeMsg archetype.id) (Archetype.viewWeight archetype)) :: cardCountElements)
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


viewAddCard : String -> Html Msg
viewAddCard currentValue =
    tr [] [ td [ class "card-cell" ] [ input [ value currentValue, onInput SetAddCardValue, onEnterDown AddCard, placeholder "Add new card..." ] [ text "+ Add Card" ] ] ]


onEnterDown : Msg -> Attribute Msg
onEnterDown msg =
    let
        keyMapper code =
            case code of
                13 ->
                    AddCard

                _ ->
                    NoOp
    in
        onKeyDown keyMapper


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (JD.map tagger keyCode)


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
            [ type_ "number"
            , value viewValue
            , onInput (\input -> saveCountMsg (Result.withDefault 0 (String.toInt input)))
            , onClick (SelectText elementId)
            , id elementId
            ]
            []


archetypeSlotInput : Slot -> String -> String -> String -> Html Msg
archetypeSlotInput slot currentValue initialEditValue elementId =
    input
        [ type_ "text"
        , onFocus (EditArchetypeSlot slot initialEditValue)
        , onInput (EditArchetypeSlot slot)
        , onBlur (CommitArchetypeSlot)
        , onClick (SelectText elementId)
        , value currentValue
        , id elementId
        ]
        []


keyedTable : List (Attribute a) -> List ( String, Html a ) -> Html a
keyedTable =
    Html.Keyed.node "table"

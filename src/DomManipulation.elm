module DomManipulation exposing (..)

import ID exposing (ID)
import Slot exposing (..)


type Element
    = MaindeckInput ID
    | SideboardInput ID
    | CardNameInput ID
    | MatchupInput Slot
    | ArchetypeSideboardPlanAnchor ID


targetId : Element -> String
targetId element =
    case element of
        MaindeckInput cardId ->
            "maindeck-input-for-card-" ++ toString cardId

        SideboardInput cardId ->
            "sideboard-input-for-card-" ++ toString cardId

        CardNameInput cardId ->
            "card-name-input" ++ toString cardId

        MatchupInput { cardId, archetypeId } ->
            "maindeck-input-for-card-" ++ toString cardId ++ "-archetype-" ++ toString archetypeId

        ArchetypeSideboardPlanAnchor archetypeId ->
            "sideboard-plan-" ++ toString archetypeId

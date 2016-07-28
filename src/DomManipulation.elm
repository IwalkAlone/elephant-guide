module DomManipulation exposing (..)

import Ports exposing (focusAndSelect)
import ID exposing (ID)


type Element
    = MaindeckInput ID
    | SideboardInput ID
    | MatchupInput ID ID


targetId : Element -> String
targetId element =
    case element of
        MaindeckInput cardId ->
            "maindeck-input-for-card-" ++ toString cardId

        SideboardInput cardId ->
            "sideboard-input-for-card-" ++ toString cardId

        MatchupInput cardId archetypeId ->
            "maindeck-input-for-card-" ++ toString cardId ++ "-archetype-" ++ toString archetypeId


focusAndSelect : Element -> Cmd msg
focusAndSelect element =
    Ports.focusAndSelect (targetId element)

module Slot exposing (..)

import ID exposing (ID)
import String


type alias Slot =
    { cardId : ID
    , archetypeId : ID
    }


display : Int -> Int -> String
display countOnThePlay drawDifference =
    if drawDifference == 0 then
        if countOnThePlay == 0 then
            ""
        else
            toString countOnThePlay
    else
        toString countOnThePlay ++ " / " ++ toString (countOnThePlay + drawDifference)


parsePlayDraw : String -> ( Int, Int )
parsePlayDraw slotString =
    let
        split =
            slotString |> String.split " " |> String.join "" |> String.split "/"
    in
        case split of
            [] ->
                Debug.crash "Split should not return empty list"

            head :: tail ->
                let
                    countOnThePlay =
                        String.toInt head
                in
                    case countOnThePlay of
                        Ok count ->
                            case String.toInt (String.join "" tail) of
                                Ok countOnTheDraw ->
                                    ( count, countOnTheDraw - count )

                                Err _ ->
                                    ( count, 0 )

                        Err _ ->
                            ( 0, 0 )

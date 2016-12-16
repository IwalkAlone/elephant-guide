module Components.DeckSettings exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as JD
import Material.Textfield as Textfield
import String
import Components.Deck.Model as Model exposing (..)
import Components.Deck.Update as Update exposing (..)


view : Model -> Html Msg
view { mdl, name, notes, targetSize } =
    div [ class "deck-settings" ]
        [ Textfield.render Mdl
            [ 3, 0 ]
            mdl
            [ Textfield.value name
            , Textfield.label "Deck Name"
            , Textfield.floatingLabel
            , Textfield.onInput EditDeckName
            , Textfield.maxlength 64
            ]
        , Textfield.render Mdl
            [ 3, 1 ]
            mdl
            [ Textfield.textarea
            , Textfield.rows 8
            , Textfield.value notes
            , Textfield.label "Notes"
            , Textfield.floatingLabel
            , Textfield.onInput EditNotes
            , Textfield.maxlength 4000
            ]
        , Textfield.render Mdl
            [ 3, 2 ]
            mdl
            [ Textfield.value (toString targetSize)
            , Textfield.label ("Target Maindeck Size")
            , Textfield.floatingLabel
            , onValidInput (String.toInt) EditTargetSize
            , Textfield.maxlength 8
            ]
        ]


onValidInput : (String -> Result String a) -> (a -> msg) -> Textfield.Property msg
onValidInput validator msg =
    Textfield.on "input" <| JD.map (msg) (Html.Events.targetValue |> JD.andThen (succeedIfValid validator))


succeedIfValid : (String -> Result String a) -> String -> JD.Decoder a
succeedIfValid validator string =
    case validator string of
        Ok validValue ->
            JD.succeed validValue

        Err _ ->
            JD.fail "Invalid input"

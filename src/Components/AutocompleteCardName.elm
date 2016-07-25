module Components.AutocompleteCardName exposing (..)

import Autocomplete exposing (..)
import Components.Card as Card exposing (..)
import Html exposing (..)


type alias Model =
    { card : Card.Model
    , autocomplete : Autocomplete
    }


type Msg
    = CardNameChanged String


init : Card.Model -> Model
init card =
    { card = card
    , autocomplete = Autocomplete.init [ "Air Elemental", "Ancestral Recall", "Animate Artifact", "Animate Dead", "Animate Wall" ]
    }


view : Model -> Html Msg
view model =
    Autocomplete.view model.autocomplete


update : Autocomplete.Msg -> Model -> ( Model, Cmd Autocomplete.Msg )
update msg model =
    Autocomplete.update msg model

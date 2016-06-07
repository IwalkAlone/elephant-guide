module Components.Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { name : String
    }


type Msg
    = EditName String


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditName newName ->
            { model | name = newName }


view : Model -> Html Msg
view model =
    div [ class "card" ]
        [ input [ class "card-name", type' "text", value model.name, onInput EditName ] []
        ]

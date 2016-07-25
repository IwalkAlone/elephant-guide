module Components.Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as JE exposing (..)


type alias Model =
    { id : Int
    , name : String
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


encoder : Model -> JE.Value
encoder card =
    JE.object
        [ ( "id", JE.int card.id )
        , ( "name", JE.string card.name )
        ]

module Components.Archetype exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toFloat)


type alias Model =
    { name : String
    , weight : Float
    }


type Msg
    = EditName String
    | EditWeight Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditName newName ->
            { model | name = newName }

        EditWeight newWeight ->
            { model | weight = newWeight }


view : Model -> Html Msg
view model =
    div [ class "archetype" ]
        [ input [ class "archetype-name", type' "text", value model.name, onInput EditName ] []
        , hr [] []
        , input
            [ class "archetype-weight"
            , type' "number"
            , value (toString model.weight)
            , onInput (\input -> EditWeight (Result.withDefault 0 (String.toFloat input)))
            ]
            []
        ]

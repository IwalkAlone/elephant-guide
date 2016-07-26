module Components.Archetype exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toFloat)
import Components.Decklist as Decklist exposing (..)
import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing ((:=))


type alias Model =
    { id : Int
    , name : String
    , weight : Float
    , decklist : Decklist
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


viewName : Model -> Html Msg
viewName model =
    input [ class "archetype-name", type' "text", value model.name, onInput EditName ] []


viewWeight : Model -> Html Msg
viewWeight model =
    input
        [ class "archetype-weight"
        , type' "number"
        , value (toString model.weight)
        , onInput (\input -> EditWeight (Result.withDefault 0 (String.toFloat input)))
        ]
        []


encoder : Model -> JE.Value
encoder archetype =
    JE.object
        [ ( "id"
          , JE.int archetype.id
          )
        , ( "name"
          , JE.string archetype.name
          )
        , ( "weight"
          , JE.float archetype.weight
          )
        , ( "decklist"
          , Decklist.encoder archetype.decklist
          )
        ]


decoder : JD.Decoder Model
decoder =
    JD.object4 Model
        ("id" := JD.int)
        ("name" := JD.string)
        ("weight" := JD.float)
        ("decklist" := Decklist.decoder)

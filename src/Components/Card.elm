module Components.Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing (..)
import ID exposing (..)


type alias Model =
    { id : ID
    , name : String
    , editing : Bool
    , currentText : String
    }


type Msg
    = StartEditing
    | Input String
    | FinishEditing


initialModel : ID -> String -> Model
initialModel id name =
    { id = id
    , name = name
    , editing = False
    , currentText = name
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartEditing ->
            { model | editing = True }

        Input input ->
            { model | currentText = input }

        FinishEditing ->
            { model | name = model.currentText, editing = False }


view : Model -> Html Msg
view model =
    div [ class "card" ]
        [ if model.editing then
            input [ class "card-name", type' "text", defaultValue model.name, onInput Input, onBlur FinishEditing ] []
          else
            span [ class "card-name", onDoubleClick StartEditing ] [ text model.name ]
        ]


encoder : Model -> JE.Value
encoder card =
    JE.object
        [ ( "id", JE.int card.id )
        , ( "name", JE.string card.name )
        ]


decoder : Decoder Model
decoder =
    JD.object2 initialModel ("id" := JD.int) ("name" := JD.string)

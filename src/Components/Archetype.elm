module Components.Archetype exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, type', value)
import Html.Events exposing (..)
import String exposing (toFloat)
import Components.Decklist as Decklist exposing (..)
import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (required, hardcoded, decode)
import Dict exposing (..)
import ID exposing (..)
import Slot exposing (..)


type alias Model =
    { id : Int
    , name : String
    , weight : Float
    , decklist : Decklist
    , differenceOnTheDraw :
        Decklist
        --, slotInputContents : Dict ID String
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
    input [ class "archetype-name", type' "text", Html.Attributes.value model.name, onInput EditName ] []


viewWeight : Model -> Html Msg
viewWeight model =
    input
        [ class "archetype-weight"
        , type' "number"
        , Html.Attributes.value (toString model.weight)
        , onInput (\input -> EditWeight (Result.withDefault 0 (String.toFloat input)))
        ]
        []


setCardCounts : Model -> ID -> ( Int, Int ) -> Model
setCardCounts archetype cardId ( countOnThePlay, differenceOnTheDraw ) =
    { archetype | decklist = Dict.insert cardId countOnThePlay archetype.decklist, differenceOnTheDraw = Dict.insert cardId differenceOnTheDraw archetype.differenceOnTheDraw }


slotDisplayValue : Model -> ID -> String
slotDisplayValue archetype cardId =
    Slot.display
        (Decklist.slotValue
            archetype.decklist
            cardId
        )
        (Decklist.slotValue
            archetype.differenceOnTheDraw
            cardId
        )


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
        , ( "differenceOnTheDraw"
          , Decklist.encoder archetype.differenceOnTheDraw
          )
        ]


decoder : JD.Decoder Model
decoder =
    decode Model
        |> required "id" JD.int
        |> required "name" JD.string
        |> required "weight" JD.float
        |> required "decklist" Decklist.decoder
        |> required "differenceOnTheDraw" Decklist.decoder

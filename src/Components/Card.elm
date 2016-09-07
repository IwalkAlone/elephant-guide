module Components.Card exposing (..)

import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline exposing (required, hardcoded, decode)
import ID exposing (..)


type alias Model =
    { id : ID
    , name : String
    }


encoder : Model -> JE.Value
encoder card =
    JE.object
        [ ( "id", JE.int card.id )
        , ( "name", JE.string card.name )
        ]


decoder : Decoder Model
decoder =
    decode Model
        |> required "id" JD.int
        |> required "name" JD.string

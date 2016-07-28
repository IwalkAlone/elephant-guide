port module Ports exposing (..)

import TableMetrics exposing (..)
import Json.Encode as JE exposing (..)


port saveDeck : JE.Value -> Cmd msg


port loadDeck : (JE.Value -> msg) -> Sub msg


port requestTableMetrics : () -> Cmd msg


port receiveTableMetrics : (TableMetrics -> msg) -> Sub msg


port focusAndSelect : String -> Cmd msg

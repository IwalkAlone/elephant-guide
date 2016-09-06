module Components.DeckSettings exposing (..)

import Html exposing (..)
import Material
import Material.Textfield


view : { a | name : String, notes : String, targetSize : int } -> Html msg
view { name, notes, targetSize } =
    text name

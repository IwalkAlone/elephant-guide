module Components.DecklistGrid exposing (..)

import Html exposing (..)
import Components.Archetype as Archetype
import Material
import Material.Grid as Grid
import Material.Options
import Material.Color


type alias Model =
    { archetypes : List Archetype.Model
    , mdl : Material.Model
    }


model : Model
model =
    { archetypes = []
    , mdl = Material.model
    }


type Msg
    = MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MDL mdlMsg ->
            Material.update MDL mdlMsg model


view =
    2

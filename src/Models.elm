port module Models exposing (..)

import Bootstrap.Accordion as Accordion
import Focus exposing (Focus)
import FormattedValue exposing (FormattedValue)
import TiltakStates exposing (TiltakStates)


type Group
    = Belysning
    | Vedlikehold


type Page
    = Home
    | GroupPage Group
    | NotFound


type alias Model =
    { page : Page
    , accordionState : Accordion.State
    , tiltakStates : TiltakStates
    , chartIds : List String
    }


type alias Field =
    { name : String
    , title : String
    , placeholder : String
    , stepSize : Float
    , value : TiltakStates -> Maybe Float
    , isEditable : TiltakStates -> Bool
    , beDisplayMode : TiltakStates -> TiltakStates
    , beEditMode : TiltakStates -> TiltakStates
    , focus : Focus TiltakStates (FormattedValue Float)
    }

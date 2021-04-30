module Field exposing (..)

import Focus exposing ((=>), Focus)
import FormattedValue exposing (Editable(..), FormattedValue, state, value)
import TiltakStates exposing (TiltakStates)


type FieldSpec
    = FloatSpec { stepSize : Int }
    | IntSpec { stepSize : Int }
    | PercentSpec


type alias Field =
    { name : String
    , title : String
    , placeholder : String
    , fieldSpec : FieldSpec
    , value : TiltakStates -> Maybe Float
    , isEditable : TiltakStates -> Bool
    , beDisplayMode : TiltakStates -> TiltakStates
    , beEditMode : TiltakStates -> TiltakStates
    , focus : Focus TiltakStates (FormattedValue Float)
    }

module TiltakAndGroupData exposing (..)

import Models exposing (..)
import Tiltak exposing (Tiltak)
import Tiltak.GsB_GsA as GsB_GsA
import Tiltak.LEDLys as LEDLys


alleTyper : List Group
alleTyper =
    [ GruppoA
    ]


tiltakForGroup : Group -> List Tiltak
tiltakForGroup gruppeType =
    case gruppeType of
        GruppoA ->
            [ GsB_GsA.tiltak, LEDLys.tiltak ]


initialTiltakStates =
    { ledLys = LEDLys.initialState
    , gsB_GsA = GsB_GsA.initialState
    }

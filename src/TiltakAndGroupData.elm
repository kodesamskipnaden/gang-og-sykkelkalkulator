module TiltakAndGroupData exposing (..)

import Models exposing (..)
import Tiltak exposing (Tiltak)
import Tiltak.GsB_GsA as GsB_GsA
import Tiltak.LEDLys as LEDLys
import Tiltak.Renhold as Renhold
import Tiltak.Vegdekkestandard as Vegdekkestandard
import TiltakStates exposing (TiltakStates)


alleTyper : List Group
alleTyper =
    [ GruppoA
    ]


tiltakForGroup : Group -> List Tiltak
tiltakForGroup gruppeType =
    case gruppeType of
        GruppoA ->
            [ GsB_GsA.tiltak, LEDLys.tiltak, Vegdekkestandard.tiltak, Renhold.tiltak ]


initialTiltakStates : TiltakStates
initialTiltakStates =
    { ledLys = LEDLys.initialState
    , gsB_GsA = GsB_GsA.initialState
    , vegdekkestandard = Vegdekkestandard.initialState
    , renhold = Renhold.initialState
    }

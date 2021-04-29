module TiltakAndGroupData exposing (..)

import Models exposing (..)
import Tiltak exposing (Tiltak)
import Tiltak.LEDLys as LEDLys
import Tiltak.Renhold as Renhold
import Tiltak.Vegdekkestandard as Vegdekkestandard
import Tiltak.Vinterdrift as Vinterdrift
import TiltakStates exposing (TiltakStates)


alleTyper : List Group
alleTyper =
    [ GruppoA
    ]


tiltakForGroup : Group -> List Tiltak
tiltakForGroup gruppeType =
    case gruppeType of
        GruppoA ->
            [ Vinterdrift.tiltak, LEDLys.tiltak, Vegdekkestandard.tiltak, Renhold.tiltak ]


initialTiltakStates : TiltakStates
initialTiltakStates =
    { ledLys = LEDLys.initialState
    , vinterdrift = Vinterdrift.initialState
    , vegdekkestandard = Vegdekkestandard.initialState
    , renhold = Renhold.initialState
    }

module TiltakAndGroupData exposing (..)

import Models exposing (..)
import Tiltak exposing (Tiltak)
import Tiltak.GsB_GsA as GsB_GsA
import Tiltak.LEDLys as LEDLys


alleTyper : List Group
alleTyper =
    [ Belysning
    , Vedlikehold
    ]



-- TODO: organize simple tiltak in a Collection with tiltak and
-- initial state paired together


tiltakForGroup : Group -> List Tiltak
tiltakForGroup gruppeType =
    case gruppeType of
        Belysning ->
            [ LEDLys.tiltak ]

        Vedlikehold ->
            [ GsB_GsA.tiltak ]


initialTiltakStates =
    { ledLys = LEDLys.initialState
    , gsB_GsA = GsB_GsA.initialState
    }


alleTiltak : List Tiltak
alleTiltak =
    [ LEDLys.tiltak, GsB_GsA.tiltak ]

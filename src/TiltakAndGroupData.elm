module TiltakAndGroupData exposing (..)

import Models exposing (..)
import Tiltak exposing (Tiltak)
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
            []


initialTiltakStates =
    { ledLys = LEDLys.initialState
    }

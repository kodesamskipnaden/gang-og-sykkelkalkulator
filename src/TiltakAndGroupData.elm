module TiltakAndGroupData exposing (..)

import Models exposing (..)
import Tiltak exposing (Tiltak)
import Tiltak.KollektivPrioriteringLyskryss as KollektivPrioriteringLyskryss
import Tiltak.LEDLys as LEDLys
import Tiltak.OpphoeyetHoldeplass as OpphoeyetHoldeplass


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
    { kollektivPrioriteringLyskryss = KollektivPrioriteringLyskryss.initialState
    , opphoeyetHoldeplass = OpphoeyetHoldeplass.initialState
    }

module TiltakForutsetninger exposing (..)

import BasicState exposing (..)
import GeneralForutsetninger exposing (verdisettinger)
import Tiltak exposing (..)


type alias StedsForutsetninger =
    { koekostnadBiler : Float
    , overfoertFra :
        { bil : Float
        , kollektivtransport : Float
        , genererte : Float
        }
    , tsKostnader :
        { bil : Float
        , kollektivtransport : Float
        }
    , oevrigeEksterneKostnader :
        { bil : Float
        , kollektivtransport : Float
        }
    }


stedsForutsetninger :
    Sted
    -> StedsForutsetninger
stedsForutsetninger sted =
    case sted of
        Storby ->
            { overfoertFra =
                { bil = 30 / 100
                , kollektivtransport = 50 / 100
                , genererte = 20 / 100
                }
            , tsKostnader =
                { bil = 0.163979652
                , kollektivtransport = 0.020346472
                }
            , oevrigeEksterneKostnader =
                { bil = 0.6207036
                , kollektivtransport = 0.250575631
                }
            , koekostnad =
                { bil = 1.3171136
                , kollektivtransport = 0.069319672
                }
            }

        LitenBy ->
            { overfoertFra =
                { bil = 50 / 100
                , kollektivtransport = 30 / 100
                , genererte = 20 / 100
                }
            , tsKostnader =
                { bil = 0.161355977
                , kollektivtransport = 0.02447308
                }
            , oevrigeEksterneKostnader =
                { bil = 0.355979021
                , kollektivtransport = 0.139331744
                }
            , koekostnad =
                { bil = 0.168
                , kollektivtransport = 0.011830986
                }
            }

        Spredtbygd ->
            { overfoertFra =
                { bil = 70 / 100
                , kollektivtransport = 20 / 100
                , genererte = 10 / 100
                }
            , tsKostnader =
                { bil = 0.155149978
                , kollektivtransport = 0.043439718
                }
            , oevrigeEksterneKostnader =
                { bil = 0.1
                , kollektivtransport = 0.072136348
                }
            , koekostnad =
                { bil = 0
                , kollektivtransport = 0
                }
            }


overfoertFraHelper (Tiltak object) state =
    let
        basicState =
            object.basicState state
    in
    (stedsForutsetninger basicState.sted).overfoertFra


basicSyklistForutsetninger :
    Tiltak
    -> TiltakStates
    -> BrukerForutsetninger
basicSyklistForutsetninger ((Tiltak object) as this) state =
    let
        overfoert =
            overfoertFraHelper this state

        receiver =
            bindTiltak this state
    in
    { andelNyeBrukereFraBil = overfoert.bil
    , andelNyeBrukereFraKollektivtransport = overfoert.kollektivtransport
    , andelNyeBrukereGenererte = overfoert.genererte
    , tsKostnad = verdisettinger.tsKostnadSykkel
    , turerPerYearMaybe = (object.basicState state).sykkelturerPerYear.value
    , totalReiseDistanceKm = verdisettinger.syklistTotalReiseDistanceKm
    , helseGevinstBruker = verdisettinger.helseTSGevinstSykkel
    , voTBruker = verdisettinger.voTSykkel
    , tidsbesparelseMinPerTur = receiver .tidsbesparelseMinPerTurSyklende
    , tsGevinstTiltak = 0
    }


basicFotgjengerForutsetninger :
    Tiltak
    -> TiltakStates
    -> BrukerForutsetninger
basicFotgjengerForutsetninger ((Tiltak object) as this) state =
    let
        overfoert =
            overfoertFraHelper this state

        receiver =
            bindTiltak this state
    in
    { andelNyeBrukereFraBil = overfoert.bil
    , andelNyeBrukereFraKollektivtransport = overfoert.kollektivtransport
    , andelNyeBrukereGenererte = overfoert.genererte
    , tsKostnad = verdisettinger.tsKostnadGange
    , totalReiseDistanceKm = verdisettinger.fotgjengerTotalReiseDistanceKm
    , helseGevinstBruker = verdisettinger.helseGevinstGange
    , turerPerYearMaybe = (object.basicState state).gangturerPerYear.value
    , voTBruker = verdisettinger.voTGange
    , tidsbesparelseMinPerTur = receiver .tidsbesparelseMinPerTurGaaende
    , tsGevinstTiltak = 0
    }

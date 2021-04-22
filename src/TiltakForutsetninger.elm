module TiltakForutsetninger exposing (..)

import BasicState exposing (..)
import GeneralForutsetninger exposing (verifiserteVerdisettinger)
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
                { bil = 0.0981818
                , kollektivtransport = 0.0203143
                }
            , oevrigeEksterneKostnader =
                { bil = 0.6207036
                , kollektivtransport = 0.2641275
                }
            , koekostnadBiler = 1.3171136
            }

        Spredtbygd ->
            { overfoertFra =
                { bil = 70 / 100
                , kollektivtransport = 20 / 100
                , genererte = 10 / 100
                }
            , tsKostnader =
                { bil = 0.092307692
                , kollektivtransport = 0.036
                }
            , oevrigeEksterneKostnader =
                { bil = 0.1
                , kollektivtransport = 0.072136348
                }
            , koekostnadBiler = 0.0
            }

        LitenBy ->
            { overfoertFra =
                { bil = 50 / 100
                , kollektivtransport = 30 / 100
                , genererte = 20 / 100
                }
            , tsKostnader =
                { bil = 0.096503497
                , kollektivtransport = 0.023581731
                }
            , oevrigeEksterneKostnader =
                { bil = 0.355979021
                , kollektivtransport = 0.160726336
                }
            , koekostnadBiler = 0.17972028
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
    , tsKostnad = verifiserteVerdisettinger.tsKostnadSykkel
    , turerPerYearMaybe = (object.basicState state).sykkelturerPerYear.value
    , totalReiseDistanceKm = verifiserteVerdisettinger.syklistTotalReiseDistanceKm
    , helseGevinstBruker = verifiserteVerdisettinger.helseTSGevinstSykkel
    , voTBruker = verifiserteVerdisettinger.voTSykkel
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
    , tsKostnad = verifiserteVerdisettinger.tsKostnadGange
    , totalReiseDistanceKm = verifiserteVerdisettinger.fotgjengerTotalReiseDistanceKm
    , helseGevinstBruker = verifiserteVerdisettinger.helseGevinstGange
    , turerPerYearMaybe = (object.basicState state).gangturerPerYear.value
    , voTBruker = verifiserteVerdisettinger.voTGange
    , tidsbesparelseMinPerTur = receiver .tidsbesparelseMinPerTurGaaende
    , tsGevinstTiltak = 0
    }

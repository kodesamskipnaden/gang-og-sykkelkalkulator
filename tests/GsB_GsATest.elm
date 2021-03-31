module GsB_GsATest exposing (..)

import BasicState exposing (..)
import BasicTiltak exposing (yearlyOverfoerteSykkelturer)
import Expect exposing (FloatingPointTolerance(..))
import FormattedValue exposing (formattedValue)
import Maybe.Extra
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (analyse, sendTo)
import Tiltak.GsB_GsA as GsB_GsA exposing (tiltak)
import TiltakAndGroupData


tiltakSuiteInProgress checkWithState expectedRecord =
    Test.concat
        [ describe "nytte calculcations"
            [ checkWithState
                "yearlySyklistNytte"
                .yearlySyklistNytte
                (Expect.within (Absolute 0.01) expectedRecord.yearlySyklistNytte)
            , checkWithState
                "yearlySyklistNytteInklOverfoert"
                .yearlySyklistNytteInklOverfoert
                (closeTo expectedRecord.yearlySyklistNytteInklOverfoert 2)
            , checkWithState
                "yearlyTrafikantNytte"
                .yearlyTrafikantNytte
                (closeTo expectedRecord.yearlyTrafikantNytte 2)
            , checkWithState
                "yearlyTrafikantNytteInklOverfoert"
                .yearlyTrafikantNytteInklOverfoert
                (closeTo expectedRecord.yearlyTrafikantNytteInklOverfoert 2)
            , checkWithState
                "yearlyHelsegevinstNytteInklOverfoert"
                .yearlyHelsegevinstNytteInklOverfoert
                (closeTo expectedRecord.yearlyHelsegevinstNytteInklOverfoert 2)
            , checkWithState
                "yearlyTSGevinstNytte"
                .yearlyTSGevinstNytte
                (closeTo expectedRecord.yearlyTSGevinstNytte 2)
            , checkWithState
                "yearlyTSGevinstNytteInklOverfoert"
                .yearlyTSGevinstNytteInklOverfoert
                (closeTo expectedRecord.yearlyTSGevinstNytteInklOverfoert 2)
            , checkWithState
                "yearlyEksterneEffekterNytteInklOverfoert"
                .yearlyEksterneEffekterNytteInklOverfoert
                (closeTo expectedRecord.yearlyEksterneEffekterNytteInklOverfoert 2)
            ]
        ]


initialState =
    TiltakAndGroupData.initialTiltakStates


sykkelSuite : Test
sykkelSuite =
    let
        state =
            { initialState
                | gsB_GsA =
                    { nivaa = LavTilHoey
                    , sted = Storby
                    , installationCost = Just 0 |> formattedValue
                    , yearlyMaintenance = Just 222000 |> formattedValue
                    , sykkelturerPerYear = Just 5.0e4 |> formattedValue
                    , gangturerPerYear = Just 0 |> formattedValue
                    , lengdeVeiKm = Just 1 |> formattedValue
                    , oppetidPercent = Just 0.8 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlySyklistNytte = 49156.67
            , yearlySyklistNytteInklOverfoert = 50385.58
            , yearlyTrafikantNytte = 0
            , yearlyTrafikantNytteInklOverfoert = 8356.14
            , yearlyHelsegevinstNytteInklOverfoert = 228000
            , yearlyTSGevinstNytte = 1403.56
            , yearlyTSGevinstNytteInklOverfoert = -32586.05
            , yearlyEksterneEffekterNytteInklOverfoert = 2514.2
            , driftOgVedlihKost = -4393995.8
            , investeringsKostInklRestverdi = 0
            , kostUtenSkyggepris = -4393995.8
            , nettoNytte = -4038217.4327044
            , nettoNytteInklOverfoert = 994558.98
            , nytte = 1234577.53
            , nytteInklOverfoert = 6267353.95
            , skyggepris = -878799.16
            }

        checkWithState : CheckWithStateFunction
        checkWithState description accessor expectation =
            test description <|
                \() ->
                    sendTo
                        tiltak
                        accessor
                        state
                        |> checkMaybe expectation
    in
    describe "GsB_GsA sykkelvei"
        [ tiltakSuite checkWithState expectedRecord
        , test "overfoerteSykkelturer" <|
            \() ->
                yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (Expect.equal 2500)
        ]


fotgjengerSuite : Test
fotgjengerSuite =
    let
        state =
            { initialState
                | gsB_GsA =
                    { nivaa = LavTilHoey
                    , sted = Storby
                    , installationCost = Just 0 |> formattedValue
                    , yearlyMaintenance = Just 2.22e5 |> formattedValue
                    , sykkelturerPerYear = Just 0 |> formattedValue
                    , gangturerPerYear = Just 5.0e4 |> formattedValue
                    , lengdeVeiKm = Just 1 |> formattedValue
                    , oppetidPercent = Just 0.8 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlySyklistNytte = 0
            , yearlySyklistNytteInklOverfoert = 0
            , yearlyFotgjengerNytteInklOverfoert = 248552.97
            , yearlyTrafikantNytte = 0 -- denne er nok feil, men vet ikke hva den skal være
            , yearlyTrafikantNytteInklOverfoert = 1975.6705
            , yearlyHelsegevinstNytteInklOverfoert = 306745.6573
            , yearlyTSGevinstNytte = 0 -- denne er feil men vet ikke fasit
            , yearlyTSGevinstNytteInklOverfoert = 32697.4675
            , yearlyEksterneEffekterNytteInklOverfoert = 1591.3741
            , nytte = 0 -- denner er feil men mangler fasit
            , nytteInklOverfoert = 14444763.2819
            , driftOgVedlihKost = -4393995.8
            , investeringsKostInklRestverdi = 0
            , kostUtenSkyggepris = -4393995.8
            , skyggepris = -878799.16
            , nettoNytte = -2901078.26
            , nettoNytteInklOverfoert = 9171968.3193
            }

        checkWithState : CheckWithStateFunction
        checkWithState description accessor expectation =
            test description <|
                \() ->
                    sendTo
                        tiltak
                        accessor
                        state
                        |> checkMaybe expectation
    in
    describe "GsB_GsA fotgjengervei"
        [ tiltakSuite checkWithState expectedRecord
        , test "overfoerteGangturer" <|
            \() ->
                yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (Expect.equal 2500)
        ]

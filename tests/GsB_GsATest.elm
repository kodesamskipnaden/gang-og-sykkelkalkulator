module GsB_GsATest exposing (..)

import BasicState exposing (..)
import BasicTiltak
import Expect exposing (FloatingPointTolerance(..))
import FormattedValue exposing (formattedValue)
import Maybe.Extra
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (Tiltak(..), analyse, sendTo)
import Tiltak.GsB_GsA as GsB_GsA exposing (tiltak)
import TiltakAndGroupData


tiltakSuiteInProgress checkWithState expectedRecord =
    Test.concat
        [ describe "nytte calculcations"
            [ checkWithState
                "yearlySyklistNytteInklOverfoert"
                .yearlySyklistNytteInklOverfoert
                (closeTo expectedRecord.yearlySyklistNytteInklOverfoert 2)
            , checkWithState
                "yearlyFotgjengerNytteInklOverfoert"
                .yearlyFotgjengerNytteInklOverfoert
                (Expect.within (Absolute 0.0001) expectedRecord.yearlyFotgjengerNytteInklOverfoert)
            , checkWithState
                "yearlyTrafikantNytteInklOverfoert"
                .yearlyTrafikantNytteInklOverfoert
                (Expect.within (Absolute 0.0001) expectedRecord.yearlyTrafikantNytteInklOverfoert)
            , checkWithState
                "yearlyHelsegevinstNytteInklOverfoert"
                .yearlyHelsegevinstNytteInklOverfoert
                (Expect.within (Absolute 0.0001) expectedRecord.yearlyHelsegevinstNytteInklOverfoert)

            -- , checkWithState
            --     "yearlyTSGevinstNytteInklOverfoert"
            --     .yearlyTSGevinstNytteInklOverfoert
            --     (closeTo expectedRecord.yearlyTSGevinstNytteInklOverfoert 2)
            -- , checkWithState
            --     "yearlyEksterneEffekterNytteInklOverfoert"
            --     .yearlyEksterneEffekterNytteInklOverfoert
            --     (closeTo expectedRecord.yearlyEksterneEffekterNytteInklOverfoert 2)
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
            , yearlyFotgjengerNytteInklOverfoert = 0
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
        [ skip <| tiltakSuite checkWithState expectedRecord
        , test "overfoerteSykkelturer" <|
            \() ->
                BasicTiltak.yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (Expect.equal 2500)
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
                    , lengdeVeiKm = Just 2.3 |> formattedValue
                    , oppetidPercent = Just 0.8 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlySyklistNytteInklOverfoert = 0
            , yearlyFotgjengerNytteInklOverfoert = 864623.7934
            , yearlyTrafikantNytteInklOverfoert = 1975.6705
            , yearlyHelsegevinstNytteInklOverfoert = 306745.6575
            , yearlyTSGevinstNytteInklOverfoert = 74363.3766
            , yearlyEksterneEffekterNytteInklOverfoert = 1591.3741
            }

        expectTiltakMaybe description accessor expectation =
            let
                (Tiltak object) =
                    tiltak
            in
            test description <|
                \() ->
                    accessor object tiltak state
                        |> checkMaybe expectation

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
        [ tiltakSuiteInProgress checkWithState expectedRecord
        , test "overfoerteGangturer" <|
            \() ->
                BasicTiltak.yearlyOverfoerteGangturer tiltak state |> checkMaybe (Expect.equal 2500)
        , test
            "tidsbesparelseMinPerTurGaaende"
          <|
            \() ->
                GsB_GsA.tidsbesparelseMinPerTurGaaende state
                    |> checkMaybe (Expect.within (Absolute 0.00001) 5.3259)
        , test
            "wtpNytte"
          <|
            \() ->
                GsB_GsA.wtpNytte tiltak state
                    |> checkMaybe (Expect.within (Absolute 0.00001) 323900)
        ]

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
                (Expect.within (Absolute 0.001) expectedRecord.yearlyHelsegevinstNytteInklOverfoert)
            , checkWithState
                "yearlyTSGevinstNytteInklOverfoert"
                .yearlyTSGevinstNytteInklOverfoert
                (Expect.within (Absolute 0.01) expectedRecord.yearlyTSGevinstNytteInklOverfoert)
            , checkWithState
                "yearlyEksterneEffekterNytteInklOverfoert"
                .yearlyEksterneEffekterNytteInklOverfoert
                (Expect.within (Absolute 0.0001) expectedRecord.yearlyEksterneEffekterNytteInklOverfoert)
            ]

        -- , describe "kost calculations"
        --     [ checkWithState
        --         "investeringsKostInklRestverdi"
        --         .investeringsKostInklRestverdi
        --         (Expect.within (Absolute 0.0001) expectedRecord.investeringsKostInklRestverdi)
        --     , checkWithState
        --         "driftOgVedlihKost"
        --         .driftOgVedlihKost
        --         (Expect.within (Absolute 0.0001) expectedRecord.driftOgVedlihKost)
        --     , checkWithState
        --         "kostUtenSkyggepris"
        --         .kostUtenSkyggepris
        --         (closeTo expectedRecord.kostUtenSkyggepris 2)
        --     , checkWithState
        --         "skyggepris"
        --         .skyggepris
        --         (Expect.within (Absolute 0.0001) expectedRecord.skyggepris)
        --     ]
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
                    , sykkelturerPerYear = Just 5.0e4 |> formattedValue
                    , gangturerPerYear = Just 0 |> formattedValue
                    , lengdeVeiKm = Just 2.3 |> formattedValue
                    , oppetidPercent = Just 0.8 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlySyklistNytteInklOverfoert = 541522.0903
            , yearlyFotgjengerNytteInklOverfoert = 0
            , yearlyTrafikantNytteInklOverfoert = 4939.1761
            , yearlyHelsegevinstNytteInklOverfoert = 291894.8659
            , yearlyTSGevinstNytteInklOverfoert = -21072.01 -- denne var veldig upresis
            , yearlyEksterneEffekterNytteInklOverfoert = 3978.4353
            , yearlyNytteInklOverfoertSum = 821262.55
            , nytteInklOverfoert = 20053553.3 -- upresis
            , skyggepris = -1775411.8173
            , driftOgVedlihKost = -8877059.0867
            , investeringsKostInklRestverdi = 0
            , kostUtenSkyggepris = -8877059.0867
            , nettoNytteInklOverfoert = 9401082.4 --upresis
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
                BasicTiltak.yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (Expect.equal 2500)
        , test "overfoerteGangturer" <|
            \() ->
                BasicTiltak.yearlyOverfoerteGangturer tiltak state |> checkMaybe (Expect.equal 0)
        , test
            "tidsbesparelseMinPerTurSyklende"
          <|
            \() ->
                GsB_GsA.tidsbesparelseMinPerTurSyklende state
                    |> checkMaybe (Expect.within (Absolute 0.00001) 2.4167)
        , test
            "wtpNytte"
          <|
            \() ->
                GsB_GsA.syklistForutsetninger tiltak state
                    |> GsB_GsA.wtpNytte tiltak state
                    |> checkMaybe (Expect.within (Absolute 0.00001) 372485)
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
            , yearlyNytteInklOverfoertSum = 1249299.8719
            , investeringsKostInklRestverdi = 0
            , driftOgVedlihKost = -8877059.0867
            , kostUtenSkyggepris = -8877059.0867
            , skyggepris = -1775411.8173
            , nytteInklOverfoert = 30505350.07
            , nettoNytteInklOverfoert = 19852879.1659
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
        [ tiltakSuite checkWithState expectedRecord
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
                GsB_GsA.fotgjengerForutsetninger tiltak state
                    |> GsB_GsA.wtpNytte tiltak state
                    |> checkMaybe (Expect.within (Absolute 0.00001) 323900)
        ]

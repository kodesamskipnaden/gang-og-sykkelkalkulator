module GsB_GsATest exposing (..)

import BasicState exposing (..)
import BasicTiltak
import Expect exposing (FloatingPointTolerance(..))
import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (Tiltak(..), analyse, sendTo)
import Tiltak.GsB_GsA as GsB_GsA exposing (tiltak)
import TiltakAndGroupData


initialState =
    TiltakAndGroupData.initialTiltakStates


basicGsBTestState =
    let
        initialGsB_GsA =
            initialState.gsB_GsA
    in
    { initialGsB_GsA
        | nivaa = LavTilHoey
        , sted = Storby
        , installationCost = Just 0 |> formattedValue
        , sykkelturerPerYear = Nothing |> formattedValue
        , gangturerPerYear = Nothing |> formattedValue
        , lengdeVeiKm = Just 2.3 |> formattedValue
        , oppetidPercent = Just 0.8 |> formattedValue
        , preferredToGraph = ""
    }


createCheckWithState state =
    let
        checkWithState description accessor expectation =
            test description <|
                \() ->
                    sendTo
                        tiltak
                        accessor
                        state
                        |> checkMaybe expectation
    in
    checkWithState



-- organiser med sub describes for hver case tror jeg.


sykkelSuite : Test
sykkelSuite =
    let
        sykkelGsBState =
            { basicGsBTestState
                | nivaa = LavTilHoey
                , sted = Storby
                , sykkelturerPerYear = Just 5.0e4 |> formattedValue
                , gangturerPerYear = Just 0 |> formattedValue
            }
    in
    describe "GsB_GsA sykkelvei"
        [ let
            state =
                { initialState
                    | gsB_GsA =
                        { sykkelGsBState
                            | nivaa = LavTilMiddels
                            , sted = Storby
                        }
                }

            expectedRecord =
                { yearlySyklistNytteInklOverfoert = 410480.3968
                , yearlyFotgjengerNytteInklOverfoert = 0
                , yearlyTrafikantNytteInklOverfoert = 3951.3409
                , yearlyHelsegevinstNytteInklOverfoert = 233515.8927
                , yearlyTSGevinstNytteInklOverfoert = -18459.6045
                , yearlyEksterneEffekterNytteInklOverfoert = 3182.7482
                , yearlyNytteInklOverfoertSum = 632670.7742
                , nytteInklOverfoert = 15448527.5142
                , driftOgVedlihKost = -1684365.0575
                , kostUtenSkyggepris = -1684365.0575
                , skyggepris = -336873.0115
                , investeringsKostInklRestverdi = 0
                , nettoNytteInklOverfoert = 13427289.4452
                }
          in
          describe "LavTilMiddels Storby"
            [ tiltakSuite (createCheckWithState state) expectedRecord ]
        , let
            state =
                { initialState
                    | gsB_GsA =
                        { sykkelGsBState
                            | nivaa = LavTilHoey
                            , sted = Storby
                        }
                }

            expectedRecord =
                { yearlySyklistNytteInklOverfoert = 541522.0903
                , yearlyFotgjengerNytteInklOverfoert = 0
                , yearlyTrafikantNytteInklOverfoert = 4939.1761
                , yearlyHelsegevinstNytteInklOverfoert = 291894.8659
                , yearlyTSGevinstNytteInklOverfoert = -21072.0181
                , yearlyEksterneEffekterNytteInklOverfoert = 3978.4353
                , yearlyNytteInklOverfoertSum = 821262.55
                , nytteInklOverfoert = 20053553.3
                , skyggepris = -1775411.8173
                , driftOgVedlihKost = -8877059.0867
                , investeringsKostInklRestverdi = 0
                , kostUtenSkyggepris = -8877059.0867
                , nettoNytteInklOverfoert = 9401082.4
                }
          in
          describe "LavTilHøy Storby"
            [ tiltakSuite (createCheckWithState state) expectedRecord
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
        ]


fotgjengerSuite : Test
fotgjengerSuite =
    let
        state =
            { initialState
                | gsB_GsA =
                    { basicGsBTestState
                        | nivaa = LavTilHoey
                        , sted = Storby
                        , sykkelturerPerYear = Just 0 |> formattedValue
                        , gangturerPerYear = Just 5.0e4 |> formattedValue
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
    in
    describe "GsB_GsA fotgjengervei"
        [ tiltakSuite (createCheckWithState state) expectedRecord
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

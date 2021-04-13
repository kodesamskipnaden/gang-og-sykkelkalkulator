module LEDLysTest exposing (..)

import BasicState exposing (..)
import BasicTiltak exposing (yearlyOverfoerteSykkelturer)
import Expect
import FormattedValue exposing (formattedValue)
import Maybe.Extra
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (analyse, sendTo)
import Tiltak.LEDLys as LEDLys exposing (tiltak)
import TiltakAndGroupData


initialState =
    TiltakAndGroupData.initialTiltakStates


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


basicLEDTestState =
    let
        initialLedLys =
            initialState.ledLys
    in
    { initialLedLys
        | nivaa = LavTilHoey
        , sted = Storby
        , installationCost = Just 0 |> formattedValue
        , sykkelturerPerYear = Nothing |> formattedValue
        , gangturerPerYear = Nothing |> formattedValue
        , lengdeVeiKm = Just 2.3 |> formattedValue
        , preferredToGraph = ""
    }


sykkelSuite : Test
sykkelSuite =
    let
        sykkelLedLysState =
            { basicLEDTestState
                | sykkelturerPerYear = Just 5.0e4 |> formattedValue
                , gangturerPerYear = Just 0 |> formattedValue
            }
    in
    describe "LEDLys sykkelvei"
        [ let
            state =
                { initialState
                    | ledLys =
                        { sykkelLedLysState
                            | nivaa = LavTilHoey
                            , sted = Storby
                        }
                }

            expectedRecord =
                { yearlySyklistNytteInklOverfoert = 354327.775
                , yearlyFotgjengerNytteInklOverfoert = 0
                , yearlyTrafikantNytteInklOverfoert = 4247.691
                , yearlyHelsegevinstNytteInklOverfoert = 251029.5847
                , yearlyTSGevinstNytteInklOverfoert = 10289.462
                , yearlyEksterneEffekterNytteInklOverfoert = 3421.454
                , yearlyNytteInklOverfoertSum = 623315.9675
                , nytteInklOverfoert = 15220102.252
                , investeringsKostInklRestverdi = 0
                , driftOgVedlihKost = -4520881.3377
                , kostUtenSkyggepris = -4520881.3377
                , skyggepris = -904176.2675
                , nettoNytteInklOverfoert = 9795044.6468
                }
          in
          describe "Storby Lav til Høy"
            [ tiltakSuite (createCheckWithState state) expectedRecord
            , test
                "overfoerte sykkelturer"
              <|
                \() ->
                    yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (Expect.equal 2150)
            ]
        ]


gangOgSykkelSuite : Test
gangOgSykkelSuite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | ledLys =
                    { nivaa = LavTilHoey
                    , sted = Storby
                    , installationCost = Just 1.0e6 |> formattedValue
                    , yearlyMaintenance = Just 0 |> formattedValue
                    , sykkelturerPerYear = Just 1.5e4 |> formattedValue
                    , gangturerPerYear = Just 2.0e4 |> formattedValue
                    , lengdeVeiKm = Just 1 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        tiltakReceiver =
            Tiltak.bindTiltak tiltak state

        expectedNytteFraFotgjengerAnalyse =
            2729345.084689

        expectedNytteFraSyklistAnalyse =
            3943913.9087

        expectedKostInklSkyggepris =
            -1.0e6 + -2.0e5

        expectedRecord =
            { yearlySyklistNytte = 18433.75
            , yearlySyklistNytteInklOverfoert = 18894.59
            , yearlyFotgjengerNytteInklOverfoert = 0
            , yearlyTrafikantNytte = 0
            , yearlyTrafikantNytteInklOverfoert =
                3133.55 + 1671.23
            , yearlyHelsegevinstNytteInklOverfoert =
                85500 + 119800
            , yearlyTSGevinstNytte = 52633.67
            , yearlyTSGevinstNytteInklOverfoert = 53045.98 - 10197.92
            , yearlyEksterneEffekterNytteInklOverfoert = 942.83 + 502.84
            , yearlyNytteInklOverfoertSum = 0 -- dette sal være summen av nyttene
            , nytte = 1735321.1
            , nytteInklOverfoert = expectedNytteFraSyklistAnalyse + expectedNytteFraFotgjengerAnalyse
            , investeringsKostInklRestverdi = -1.0e6
            , driftOgVedlihKost = 0
            , kostUtenSkyggepris = -1.0e6
            , skyggepris = -2.0e5
            , nettoNytte = 535321.1
            , nettoNytteInklOverfoert =
                expectedNytteFraSyklistAnalyse
                    + expectedNytteFraFotgjengerAnalyse
                    + expectedKostInklSkyggepris
            }

        checkWithState : CheckWithStateFunction
        checkWithState description accessor expectation =
            test description <|
                \() ->
                    tiltakReceiver accessor
                        |> checkMaybe expectation

        expectedAnalyse =
            { analysePeriode = 40
            , isProfitable = Just True
            , syklistNytte = Just 461367.3704234241
            , fotgjengerNytte = Just 0
            , trafikantNytte = Just 117322.9554847516
            , helseGevinstNytte = Just 5013006.4928190885
            , tsGevinstNytte = Just 1046261.9706246123
            , eksterneEffekterNytte = Just 35300.2084146674
            , nytte = Just 6673258.997766544
            , skyggepris = Just -200000
            , nettoNytte = Just 5473258.997766544
            , kostUtenSkyggepris = Just -1000000
            , nettoNyttePerBudsjettKrone = Just 5.4732589977665445
            }

        actualAnalyse =
            Tiltak.analyse tiltak state
    in
    describe "LEDLys gang og sykkelvei"
        [ skip <| tiltakSuite checkWithState expectedRecord
        , skip <|
            test "yearlyOverfoerteSykkelturer" <|
                \() ->
                    yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (closeTo 750 2)
        , skip <|
            test "analyse" <|
                \() ->
                    actualAnalyse
                        |> Expect.equal
                            expectedAnalyse
        , test "sum av nytte elementer" <|
            \() ->
                Maybe.Extra.combine
                    [ actualAnalyse.syklistNytte
                    , actualAnalyse.fotgjengerNytte
                    , actualAnalyse.trafikantNytte
                    , actualAnalyse.helseGevinstNytte
                    , actualAnalyse.tsGevinstNytte
                    , actualAnalyse.eksterneEffekterNytte
                    ]
                    |> Maybe.map List.sum
                    |> Expect.equal actualAnalyse.nytte
        ]


ifLengdeLongerThanAverageTrip : Test
ifLengdeLongerThanAverageTrip =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        stateLengdeLargest =
            { initialState
                | ledLys =
                    { nivaa = LavTilHoey
                    , sted = Storby
                    , installationCost = Just 1.0e6 |> formattedValue
                    , yearlyMaintenance = Just 0 |> formattedValue
                    , sykkelturerPerYear = Just 1.5e4 |> formattedValue
                    , gangturerPerYear = Just 0 |> formattedValue
                    , lengdeVeiKm = Just 6 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        stateTotalLargest =
            { initialState
                | ledLys =
                    { nivaa = LavTilHoey
                    , sted = Storby
                    , installationCost = Just 1.0e6 |> formattedValue
                    , yearlyMaintenance = Just 0 |> formattedValue
                    , sykkelturerPerYear = Just 1.5e4 |> formattedValue
                    , gangturerPerYear = Just 0 |> formattedValue
                    , lengdeVeiKm = Just 1 |> formattedValue
                    , preferredToGraph = ""
                    }
            }
    in
    skip <|
        describe "iffing"
            [ test "lengdeLargest"
                (\() ->
                    sendTo
                        tiltak
                        .yearlyTSGevinstNytteInklOverfoert
                        stateLengdeLargest
                        |> checkMaybe (closeTo 263580.65 2)
                )
            , test "totalLargest"
                (\() ->
                    sendTo
                        tiltak
                        .yearlyTSGevinstNytteInklOverfoert
                        stateTotalLargest
                        |> checkMaybe (closeTo 53045.98 2)
                )
            ]

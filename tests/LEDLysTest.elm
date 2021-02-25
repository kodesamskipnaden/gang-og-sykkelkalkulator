module LEDLysTest exposing (..)

import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (TiltakAccessor, sendTo)
import Tiltak.LEDLys as LEDLys exposing (tiltak, yearlyOverfoerteSykkelturer)
import TiltakAndGroupData


sykkelSuite : Test
sykkelSuite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | ledLys =
                    { installationCost = Just 1.0e6 |> formattedValue
                    , yearlyMaintenance = Just 0 |> formattedValue
                    , sykkelturerPerYear = Just 1.5e4 |> formattedValue
                    , gangturerPerYear = Just 0 |> formattedValue
                    , lengdeVeiKm = Just 1 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlySyklistNytte = 18433.75
            , yearlySyklistNytteInklOverfoert = 18894.59
            , yearlyTrafikantNytte = 0
            , yearlyTrafikantNytteInklOverfoert = 3133.55
            , yearlyHelsegevinstNytteInklOverfoert = 85500
            , yearlyTSGevinstNytte = 52633.67
            , yearlyTSGevinstNytteInklOverfoert = 53045.98
            , yearlyEksterneEffekterNytteInklOverfoert = 942.83
            , syklistNytte = 450114.51
            , fotgjengerNytte = 0
            , trafikantNytte = 0
            , nytte = 1735321.1
            , nytteInklOverfoert = 3943913.91
            , investeringsKostInklRestverdi = -1.0e6
            , driftOgVedlihKost = 0
            , kostUtenSkyggepris = -1.0e6
            , skyggepris = -2.0e5
            , nettoNytte = 535321.1
            , nettoNytteInklOverfoert = 2743913.91
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
    describe "LEDLys sykkelvei"
        [ tiltakSuite checkWithState expectedRecord
        , test "flupps" <|
            \() ->
                yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (closeTo 750 2)
        ]


gangOgSykkelSuite : Test
gangOgSykkelSuite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | ledLys =
                    { installationCost = Just 1.0e6 |> formattedValue
                    , yearlyMaintenance = Just 0 |> formattedValue
                    , sykkelturerPerYear = Just 1.5e4 |> formattedValue
                    , gangturerPerYear = Just 2.0e4 |> formattedValue
                    , lengdeVeiKm = Just 1 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlySyklistNytte = 18433.75
            , yearlySyklistNytteInklOverfoert = 18894.59
            , yearlyTrafikantNytte = 0
            , yearlyTrafikantNytteInklOverfoert =
                3133.55 + 1671.23
            , yearlyHelsegevinstNytteInklOverfoert =
                85500 + 119800
            , yearlyTSGevinstNytte = 52633.67 
            , yearlyTSGevinstNytteInklOverfoert = 53045.98 - 10197.92
            , yearlyEksterneEffekterNytteInklOverfoert = 942.83 + 502.84
            , syklistNytte = 450114.51
            , fotgjengerNytte = 0
            , trafikantNytte = 0
            , nytte = 1735321.1 + 2729345.08
            , nytteInklOverfoert = 3943913.91
            , investeringsKostInklRestverdi = -1.0e6
            , driftOgVedlihKost = 0
            , kostUtenSkyggepris = -1.0e6
            , skyggepris = -2.0e5
            , nettoNytte = 535321.1
            , nettoNytteInklOverfoert = 2743913.91
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
    describe "LEDLys gang og sykkelvei"
        [ tiltakSuite checkWithState expectedRecord
        , test "flupps" <|
            \() ->
                yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (closeTo 750 2)
        ]


ifLengdeLongerThanAverageTrip : Test
ifLengdeLongerThanAverageTrip =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        stateLengdeLargest =
            { initialState
                | ledLys =
                    { installationCost = Just 1.0e6 |> formattedValue
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
                    { installationCost = Just 1.0e6 |> formattedValue
                    , yearlyMaintenance = Just 0 |> formattedValue
                    , sykkelturerPerYear = Just 1.5e4 |> formattedValue
                    , gangturerPerYear = Just 0 |> formattedValue
                    , lengdeVeiKm = Just 1 |> formattedValue
                    , preferredToGraph = ""
                    }
            }
    in
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

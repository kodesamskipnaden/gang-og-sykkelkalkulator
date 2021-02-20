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
                    , lengdeSykkelveiKm = Just 1 |> formattedValue
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
    describe "LEDLys tiltakSuite"
        [ tiltakSuite checkWithState expectedRecord
        , test "flupps" <|
            \() ->
                yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (closeTo 750 2)
        ]

-- gangOgSykkelSuite : Test
-- gangOgSykkelSuite =
--     let
--         initialState =
--             TiltakAndGroupData.initialTiltakStates

--         state =
--             { initialState
--                 | ledLys =
--                     { installationCost = Just 1.0e6 |> formattedValue
--                     , yearlyMaintenance = Just 0 |> formattedValue
--                     , sykkelturerPerYear = Just 1.5e4 |> formattedValue
--                     , gangturerPerYear = Just 0 |> formattedValue
--                     , lengdeSykkelveiKm = Just 1 |> formattedValue
--                     , preferredToGraph = ""
--                     }
--             }

--         expectedRecord =
--             { yearlySyklistNytte = 18433.75
--             , yearlySyklistNytteInklOverfoert = 18894.59
--             , yearlyTrafikantNytte = 0
--             , yearlyTrafikantNytteInklOverfoert = 3133.55
--             , yearlyHelsegevinstNytteInklOverfoert = 85500
--             , yearlyTSGevinstNytte = 52633.67
--             , yearlyTSGevinstNytteInklOverfoert = 53045.98
--             , yearlyEksterneEffekterNytteInklOverfoert = 942.83
--             , syklistNytte = 450114.51
--             , trafikantNytte = 0
--             , nytte = 1735321.1
--             , nytteInklOverfoert = 3943913.91
--             , investeringsKostInklRestverdi = -1.0e6
--             , driftOgVedlihKost = 0
--             , kostUtenSkyggepris = -1.0e6
--             , skyggepris = -2.0e5
--             , nettoNytte = 535321.1
--             , nettoNytteInklOverfoert = 2743913.91
--             }

--         checkWithState : CheckWithStateFunction
--         checkWithState description accessor expectation =
--             test description <|
--                 \() ->
--                     sendTo
--                         tiltak
--                         accessor
--                         state
--                         |> checkMaybe expectation
--     in
--     describe "LEDLys tiltakSuite 2"
--         [ tiltakSuite checkWithState expectedRecord
--         , test "flupps" <|
--             \() ->
--                 yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (closeTo 750 2)
--         ]

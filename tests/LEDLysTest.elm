module LEDLysTest exposing (..)

import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (TiltakAccessor, sendTo)
import Tiltak.LEDLys as LEDLys exposing (tiltak)
import TiltakAndGroupData


suite : Test
suite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | ledLys =
                    { installationCost = Just 1.0e6 |> formattedValue
                    , yearlyMaintenance = Just 0 |> formattedValue
                    , sykkelturerPerYear = Just 1.5e4 |> formattedValue
                    , lengdeSykkelveiKm = Just 1 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlyBrukerNytte = 18433.75
            , yearlyTrafikantNytte = 0
            , yearlyTSGevinstNytte = 52633.67
            , brukerNytte = 450114.51
            , trafikantNytte = 0
            , nytte = 1735321.1
            , investeringsKostInklRestverdi = -1.0e6
            , driftOgVedlihKost = 0
            , kostUtenSkyggepris = -1.0e6
            , skyggepris = -2.0e5
            , nettoNytte = 535321.1
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
        ]

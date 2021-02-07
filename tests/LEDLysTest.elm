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
            , brukerNytte = 414317.28
            , trafikantNytte = 0
            , nytte = 1735321.1
            , investeringsKostInklRestverdi = -2.44
            , driftOgVedlihKost = -39.59
            , kostUtenSkyggepris = -42.03
            , skyggepris = -8.4052
            , nettoNytte = 1547.75
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

module VegdekkestandardTest exposing (..)

import BasicState exposing (..)
import Expect exposing (FloatingPointTolerance(..))
import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (Tiltak(..), analyse, sendTo)
import Tiltak.Vegdekkestandard as Vegdekkestandard exposing (tiltak)
import TiltakAndGroupData
import TiltakSupport


initialState =
    TiltakAndGroupData.initialTiltakStates


basicVegdekkeTestState =
    let
        initialVegdekkestandard =
            initialState.vegdekkestandard
    in
    { initialVegdekkestandard
        | nivaa = LavTilHoey
        , sted = Storby
        , installationCost = Just 0 |> formattedValue
        , sykkelturerPerYear = Nothing |> formattedValue
        , gangturerPerYear = Nothing |> formattedValue
        , lengdeVeiKm = Just 2.3 |> formattedValue
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



-- hva med å bruke Focus til å oppdatere state med istedet for disse lange utrykkene??


suite : Test
suite =
    describe "Vegdekkestandard"
        [ describe "Sykkelvei"
            (let
                sykkelVegdekkeState =
                    { basicVegdekkeTestState
                        | sykkelturerPerYear = Just 5.0e4 |> formattedValue
                        , gangturerPerYear = Just 0 |> formattedValue
                    }
             in
             [ describe "LavTilHøy Storby"
                (let
                    state =
                        { initialState
                            | vegdekkestandard =
                                { sykkelVegdekkeState
                                    | nivaa = LavTilHoey
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 213137.6931
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 2472.6834
                        , yearlyHelsegevinstNytteInklOverfoert = 155000
                        , yearlyTSGevinstNytteInklOverfoert = 3268.8015
                        , yearlyEksterneEffekterNytteInklOverfoert = 1910.3232
                        , yearlyNytteInklOverfoertSum = 375789.5013
                        , nytteInklOverfoert = 9176011.7384
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -1056142.4144
                        , kostUtenSkyggepris = -1056142.4144
                        , skyggepris = -211228.4829
                        , nettoNytteInklOverfoert = 7908640.8411
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             ]
            )
        ]

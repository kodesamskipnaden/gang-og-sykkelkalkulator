module VegdekkestandardTest exposing (..)

import BasicState exposing (..)
import Expect
import FormattedValue exposing (formattedValue)
import SpecificStates exposing (VegdekkestandardState)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (Tiltak(..), TiltakRecord, TiltakStates, analyse, sendTo)
import Tiltak.Vegdekkestandard as Vegdekkestandard exposing (tiltak)
import TiltakAndGroupData


initialState : TiltakStates
initialState =
    TiltakAndGroupData.initialTiltakStates


basicVegdekkeTestState : VegdekkestandardState
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


type alias CheckWithStateFunction a =
    String
    -> (TiltakRecord -> Tiltak -> TiltakStates -> Maybe a)
    -> (a -> Expect.Expectation)
    -> Test


createCheckWithState : TiltakStates -> CheckWithStateFunction a
createCheckWithState state =
    let
        checkWithState : CheckWithStateFunction a
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
             , describe "LavTilMiddels Storby"
                (let
                    state =
                        { initialState
                            | vegdekkestandard =
                                { sykkelVegdekkeState
                                    | nivaa = LavTilMiddels
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 156999.6695
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 1879.2394
                        , yearlyHelsegevinstNytteInklOverfoert = 117800
                        , yearlyTSGevinstNytteInklOverfoert = -2652.812
                        , yearlyEksterneEffekterNytteInklOverfoert = 1451.8457
                        , yearlyNytteInklOverfoertSum = 275477.9426
                        , nytteInklOverfoert = 6726608.4496
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -204855.2097
                        , kostUtenSkyggepris = -204855.2097
                        , skyggepris = -40971.0419
                        , nettoNytteInklOverfoert = 6480782.1979
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             ]
            )
        ]

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
                        , yearlyHelsegevinstNytteInklOverfoert = 92375
                        , yearlyTSGevinstNytteInklOverfoert = 3268.8015
                        , yearlyEksterneEffekterNytteInklOverfoert = 1910.3232
                        , yearlyNytteInklOverfoertSum = 313164.5013
                        , nytteInklOverfoert = 7646837.2055
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -1056142.4144
                        , kostUtenSkyggepris = -1056142.4144
                        , skyggepris = -211228.4829
                        , nettoNytteInklOverfoert = 6379466.3082
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
                        , yearlyHelsegevinstNytteInklOverfoert = 70205
                        , yearlyTSGevinstNytteInklOverfoert = -2652.812
                        , yearlyEksterneEffekterNytteInklOverfoert = 1451.8457
                        , yearlyNytteInklOverfoertSum = 227882.9425
                        , nytteInklOverfoert = 5564435.8031
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -204855.2097
                        , kostUtenSkyggepris = -204855.2097
                        , skyggepris = -40971.0419
                        , nettoNytteInklOverfoert = 5318609.5515
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             , describe "MiddelsTilHøy Storby"
                (let
                    state =
                        { initialState
                            | vegdekkestandard =
                                { sykkelVegdekkeState
                                    | nivaa = MiddelsTilHoey
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 55149.1075
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 593.444
                        , yearlyHelsegevinstNytteInklOverfoert = 22170
                        , yearlyTSGevinstNytteInklOverfoert = 6107.065
                        , yearlyEksterneEffekterNytteInklOverfoert = 458.4776
                        , yearlyNytteInklOverfoertSum = 84478.09410808914
                        , nytteInklOverfoert = 2062782.4367502816
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -851287.2047
                        , kostUtenSkyggepris = -851287.2047
                        , skyggepris = -170257.4409
                        , nettoNytteInklOverfoert = 1041237.7911
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             ]
            )
        ]

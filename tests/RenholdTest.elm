module RenholdTest exposing (..)

import BasicState exposing (..)
import Expect
import FormattedValue exposing (formattedValue)
import SpecificStates exposing (RenholdState)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (Tiltak(..), TiltakRecord, TiltakStates, analyse, sendTo)
import Tiltak.Renhold as Renhold exposing (tiltak)
import TiltakAndGroupData


initialState : TiltakStates
initialState =
    TiltakAndGroupData.initialTiltakStates


basicRenholdTestState : RenholdState
basicRenholdTestState =
    let
        initialRenhold =
            initialState.renhold
    in
    { initialRenhold
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
    describe "Renhold"
        [ describe "Sykkelvei"
            (let
                sykkelRenholdState =
                    { basicRenholdTestState
                        | sykkelturerPerYear = Just 5.0e4 |> formattedValue
                        , gangturerPerYear = Just 0 |> formattedValue
                    }
             in
             [ describe "LavTilHøy Storby"
                (let
                    state =
                        { initialState
                            | renhold =
                                { sykkelRenholdState
                                    | nivaa = LavTilHoey
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 162505.8708
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 1780.3321
                        , yearlyHelsegevinstNytteInklOverfoert = 111600
                        , yearlyTSGevinstNytteInklOverfoert = 7939.4717
                        , yearlyEksterneEffekterNytteInklOverfoert = 1375.4327
                        , yearlyNytteInklOverfoertSum = 285201.1073
                        , nytteInklOverfoert = 6964028.265
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -3503706.9365
                        , kostUtenSkyggepris = -3503706.9365
                        , skyggepris = -700741.3873
                        , nettoNytteInklOverfoert = 2759579.9412
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             , describe "LavTilMiddels Storby"
                (let
                    state =
                        { initialState
                            | renhold =
                                { sykkelRenholdState
                                    | nivaa = LavTilMiddels
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 113763.014
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 1384.7027
                        , yearlyHelsegevinstNytteInklOverfoert = 86800
                        , yearlyTSGevinstNytteInklOverfoert = 1936.5935
                        , yearlyEksterneEffekterNytteInklOverfoert = 1069.781
                        , yearlyNytteInklOverfoertSum = 204954.0912
                        , nytteInklOverfoert = 5004560.107
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -1022910.3471
                        , kostUtenSkyggepris = -1022910.3471
                        , skyggepris = -204582.0694
                        , nettoNytteInklOverfoert = 3777067.6905
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             , describe "MiddelsTilHøy Storby"
                (let
                    state =
                        { initialState
                            | renhold =
                                { sykkelRenholdState
                                    | nivaa = MiddelsTilHoey
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 48180.3234
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 395.6293
                        , yearlyHelsegevinstNytteInklOverfoert = 24800
                        , yearlyTSGevinstNytteInklOverfoert = 6318.4641
                        , yearlyEksterneEffekterNytteInklOverfoert = 305.6517
                        , yearlyNytteInklOverfoertSum = 80000.0685
                        , nytteInklOverfoert = 1953438.2026
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -2480796.5894
                        , kostUtenSkyggepris = -2480796.5894
                        , skyggepris = -496159.3179
                        , nettoNytteInklOverfoert = -1023517.7047
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             ]
            )
        ]

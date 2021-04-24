module RenholdTest exposing (..)

import BasicState exposing (..)
import Expect exposing (FloatingPointTolerance(..))
import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (Tiltak(..), analyse, sendTo)
import Tiltak.Renhold as Renhold exposing (tiltak)
import TiltakAndGroupData
import TiltakSupport


initialState =
    TiltakAndGroupData.initialTiltakStates


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
             ]
            )
        ]

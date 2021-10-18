module VinterdriftTest exposing (..)

import BasicState exposing (..)
import Expect exposing (FloatingPointTolerance(..))
import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (Tiltak(..), analyse, sendTo)
import Tiltak.Vinterdrift as Vinterdrift exposing (tiltak)
import TiltakAndGroupData
import TiltakSupport


initialState =
    TiltakAndGroupData.initialTiltakStates


basicGsBTestState =
    let
        initialVinterdrift =
            initialState.vinterdrift
    in
    { initialVinterdrift
        | nivaa = LavTilHoey
        , sted = Storby
        , installationCost = Just 0 |> formattedValue
        , sykkelturerPerYear = Nothing |> formattedValue
        , gangturerPerYear = Nothing |> formattedValue
        , lengdeVeiKm = Just 2.3 |> formattedValue
        , oppetidPercent = Just 0.8 |> formattedValue
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
    describe "Vinterdrift"
        [ describe "Sykkelvei"
            (let
                sykkelGsBState =
                    { basicGsBTestState
                        | sykkelturerPerYear = Just 5.0e4 |> formattedValue
                        , gangturerPerYear = Just 0 |> formattedValue
                    }
             in
             [ describe "MiddelsTilHøy Storby"
                (let
                    state =
                        { initialState
                            | vinterdrift =
                                { sykkelGsBState
                                    | nivaa = MiddelsTilHoey
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 112249.3499
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 989.0734
                        , yearlyHelsegevinstNytteInklOverfoert = 36950
                        , yearlyTSGevinstNytteInklOverfoert = -1092.4132
                        , yearlyEksterneEffekterNytteInklOverfoert = 764.1293
                        , yearlyNytteInklOverfoertSum = 149860.1395
                        , nytteInklOverfoert = 3659278.3837
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -7192694.0292
                        , kostUtenSkyggepris = -7192694.0292
                        , skyggepris = -1438538.8058
                        , nettoNytteInklOverfoert = -4971954.451432221
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             , describe "LavTilMiddels Storby"
                (let
                    state =
                        { initialState
                            | vinterdrift =
                                { sykkelGsBState
                                    | nivaa = LavTilMiddels
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 372910.6332
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 3956.2935
                        , yearlyHelsegevinstNytteInklOverfoert = 147800
                        , yearlyTSGevinstNytteInklOverfoert = -11077.97
                        , yearlyEksterneEffekterNytteInklOverfoert = 3056.5172
                        , yearlyNytteInklOverfoertSum = 516645.4738
                        , nytteInklOverfoert = 12615426.7633
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -1684365.0575
                        , kostUtenSkyggepris = -1684365.0575
                        , skyggepris = -336873.0115
                        , nettoNytteInklOverfoert = 10594188.6944
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             , describe "LavTilHøy Storby"
                (let
                    state =
                        { initialState
                            | vinterdrift =
                                { sykkelGsBState
                                    | nivaa = LavTilHoey
                                    , sted = Storby
                                }
                        }

                    receiver =
                        Tiltak.bindTiltak tiltak state

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 489221.7943
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 4945.3669
                        , yearlyHelsegevinstNytteInklOverfoert = 184750
                        , yearlyTSGevinstNytteInklOverfoert = -12201.0248
                        , yearlyEksterneEffekterNytteInklOverfoert = 3820.6465
                        , yearlyNytteInklOverfoertSum = 670536.7829
                        , nytteInklOverfoert = 16373138.0731
                        , skyggepris = -1775411.8173
                        , driftOgVedlihKost = -8877059.0867
                        , investeringsKostInklRestverdi = 0
                        , kostUtenSkyggepris = -8877059.0867
                        , nettoNytteInklOverfoert = 5720667.169
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord
                 , test "overfoerteSykkelturer" <|
                    \() ->
                        receiver .syklistForutsetninger
                            |> TiltakSupport.yearlyOverfoerteTurer tiltak state
                            |> checkMaybe (Expect.equal 2500)
                 , test "overfoerteGangturer" <|
                    \() ->
                        receiver .fotgjengerForutsetninger |> TiltakSupport.yearlyOverfoerteTurer tiltak state |> checkMaybe (Expect.equal 0)
                 , test
                    "tidsbesparelseMinPerTurSyklende"
                   <|
                    \() ->
                        receiver .tidsbesparelseMinPerTurSyklende
                            |> checkMaybe (Expect.within (Absolute 0.00001) 2.4167)
                 , test
                    "wtpNytte"
                   <|
                    \() ->
                        receiver .syklistForutsetninger
                            |> receiver .wtpNytte
                            |> checkMaybe (Expect.within (Absolute 0.00001) 372485)
                 ]
                )
             ]
            )
        , describe "Gangvei"
            (let
                fotgjengerGsBState =
                    { basicGsBTestState
                        | sykkelturerPerYear = Just 0 |> formattedValue
                        , gangturerPerYear = Just 5.0e4 |> formattedValue
                    }
             in
             [ describe "Liten by LavTilHøy"
                (let
                    state =
                        { initialState
                            | vinterdrift =
                                { fotgjengerGsBState
                                    | nivaa = LavTilHoey
                                    , sted = LitenBy
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 0
                        , yearlyFotgjengerNytteInklOverfoert = 888002.3328
                        , yearlyTrafikantNytteInklOverfoert = 437.7465
                        , yearlyHelsegevinstNytteInklOverfoert = 117150
                        , yearlyTSGevinstNytteInklOverfoert = 28455.7855
                        , yearlyEksterneEffekterNytteInklOverfoert = 1092.9976
                        , yearlyNytteInklOverfoertSum = 1035138.8624
                        , nytteInklOverfoert = 25275975.832210887
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -8877059.0867
                        , kostUtenSkyggepris = -8877059.0867
                        , skyggepris = -1775411.8173
                        , nettoNytteInklOverfoert = 14623504.9282
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord
                 ]
                )
             , describe "Spredtbygd LavTilHøy"
                (let
                    state =
                        { initialState
                            | vinterdrift =
                                { fotgjengerGsBState
                                    | nivaa = LavTilHoey
                                    , sted = Spredtbygd
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 0
                        , yearlyFotgjengerNytteInklOverfoert = 888002.3328
                        , yearlyTrafikantNytteInklOverfoert = 0
                        , yearlyHelsegevinstNytteInklOverfoert = 117150
                        , yearlyTSGevinstNytteInklOverfoert = 28602.1506
                        , yearlyEksterneEffekterNytteInklOverfoert = 422.1363
                        , yearlyNytteInklOverfoertSum = 1034176.6198
                        , nytteInklOverfoert = 25252479.8325
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -8877059.0867
                        , kostUtenSkyggepris = -8877059.0867
                        , skyggepris = -1775411.8173
                        , nettoNytteInklOverfoert = 14600008.9284
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             , describe "Storby LavTilHøy"
                (let
                    state =
                        { initialState
                            | vinterdrift =
                                { fotgjengerGsBState
                                    | nivaa = LavTilHoey
                                    , sted = Storby
                                }
                        }

                    receiver =
                        Tiltak.bindTiltak tiltak state

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 0
                        , yearlyFotgjengerNytteInklOverfoert = 888002.3328
                        , yearlyTrafikantNytteInklOverfoert = 1978.1467
                        , yearlyHelsegevinstNytteInklOverfoert = 117150
                        , yearlyTSGevinstNytteInklOverfoert = 28312.5216
                        , yearlyEksterneEffekterNytteInklOverfoert = 1528.2586
                        , yearlyNytteInklOverfoertSum = 1036971.2598
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -8877059.0867
                        , kostUtenSkyggepris = -8877059.0867
                        , skyggepris = -1775411.8173
                        , nytteInklOverfoert = 25320719.2307
                        , nettoNytteInklOverfoert = 14668248.3267
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord
                 , test "overfoerteGangturer" <|
                    \() ->
                        receiver .fotgjengerForutsetninger
                            |> TiltakSupport.yearlyOverfoerteTurer tiltak state
                            |> checkMaybe (Expect.equal 2500)
                 , test
                    "tidsbesparelseMinPerTurGaaende"
                   <|
                    \() ->
                        receiver .tidsbesparelseMinPerTurGaaende
                            |> checkMaybe (Expect.within (Absolute 0.00001) 5.3259)
                 , test
                    "wtpNytte"
                   <|
                    \() ->
                        receiver .fotgjengerForutsetninger
                            |> receiver .wtpNytte
                            |> checkMaybe (Expect.within (Absolute 0.00001) 323900)
                 ]
                )
             ]
            )
        ]

module GsB_GsATest exposing (..)

import BasicState exposing (..)
import Expect exposing (FloatingPointTolerance(..))
import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (Tiltak(..), analyse, sendTo)
import Tiltak.GsB_GsA as GsB_GsA exposing (tiltak)
import TiltakAndGroupData
import TiltakSupport


initialState =
    TiltakAndGroupData.initialTiltakStates


basicGsBTestState =
    let
        initialGsB_GsA =
            initialState.gsB_GsA
    in
    { initialGsB_GsA
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
    describe "GsB_GsA"
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
                            | gsB_GsA =
                                { sykkelGsBState
                                    | nivaa = MiddelsTilHoey
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 112249.3499
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 989.0734
                        , yearlyHelsegevinstNytteInklOverfoert = 62000
                        , yearlyTSGevinstNytteInklOverfoert = -1092.4132
                        , yearlyEksterneEffekterNytteInklOverfoert = 764.1293
                        , yearlyNytteInklOverfoertSum = 174910.1394
                        , nytteInklOverfoert = 4270948.1954
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -7192694.0292
                        , kostUtenSkyggepris = -7192694.0292
                        , skyggepris = -1438538.8058
                        , nettoNytteInklOverfoert = -4360284.6397
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             , describe "LavTilMiddels Storby"
                (let
                    state =
                        { initialState
                            | gsB_GsA =
                                { sykkelGsBState
                                    | nivaa = LavTilMiddels
                                    , sted = Storby
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 372910.6332
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 3956.2935
                        , yearlyHelsegevinstNytteInklOverfoert = 248000
                        , yearlyTSGevinstNytteInklOverfoert = -11077.97
                        , yearlyEksterneEffekterNytteInklOverfoert = 3056.5172
                        , yearlyNytteInklOverfoertSum = 616845.4738
                        , nytteInklOverfoert = 15062106.0175
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -1684365.0575
                        , kostUtenSkyggepris = -1684365.0575
                        , skyggepris = -336873.0115
                        , nettoNytteInklOverfoert = 13040867.9485
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             , describe "LavTilHøy Storby"
                (let
                    state =
                        { initialState
                            | gsB_GsA =
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
                        , yearlyHelsegevinstNytteInklOverfoert = 310000
                        , yearlyTSGevinstNytteInklOverfoert = -12201.0248
                        , yearlyEksterneEffekterNytteInklOverfoert = 3820.6465
                        , yearlyNytteInklOverfoertSum = 795786.7829
                        , nytteInklOverfoert = 19431487.138
                        , skyggepris = -1775411.8173
                        , driftOgVedlihKost = -8877059.0867
                        , investeringsKostInklRestverdi = 0
                        , kostUtenSkyggepris = -8877059.0867
                        , nettoNytteInklOverfoert = 8779016.2339
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
                            | gsB_GsA =
                                { fotgjengerGsBState
                                    | nivaa = LavTilHoey
                                    , sted = LitenBy
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 0
                        , yearlyFotgjengerNytteInklOverfoert = 888002.3328
                        , yearlyTrafikantNytteInklOverfoert = 437.7465
                        , yearlyHelsegevinstNytteInklOverfoert = 324500
                        , yearlyTSGevinstNytteInklOverfoert = 28455.7855
                        , yearlyEksterneEffekterNytteInklOverfoert = 1092.9976
                        , yearlyNytteInklOverfoertSum = 1242488.8624
                        , nytteInklOverfoert = 30339039.1351
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -8877059.0867
                        , kostUtenSkyggepris = -8877059.0867
                        , skyggepris = -1775411.8173
                        , nettoNytteInklOverfoert = 19686568.231
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord
                 ]
                )
             , describe "Spredtbygd LavTilHøy"
                (let
                    state =
                        { initialState
                            | gsB_GsA =
                                { fotgjengerGsBState
                                    | nivaa = LavTilHoey
                                    , sted = Spredtbygd
                                }
                        }

                    expectedRecord =
                        { yearlySyklistNytteInklOverfoert = 0
                        , yearlyFotgjengerNytteInklOverfoert = 888002.3328
                        , yearlyTrafikantNytteInklOverfoert = 0
                        , yearlyHelsegevinstNytteInklOverfoert = 324500
                        , yearlyTSGevinstNytteInklOverfoert = 28602.1506
                        , yearlyEksterneEffekterNytteInklOverfoert = 422.1363
                        , yearlyNytteInklOverfoertSum = 1241526.6197
                        , nytteInklOverfoert = 30315543.1353
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -8877059.0867
                        , kostUtenSkyggepris = -8877059.0867
                        , skyggepris = -1775411.8173
                        , nettoNytteInklOverfoert = 19663072.2313
                        }
                 in
                 [ tiltakSuite (createCheckWithState state) expectedRecord ]
                )
             , describe "Storby LavTilHøy"
                (let
                    state =
                        { initialState
                            | gsB_GsA =
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
                        , yearlyHelsegevinstNytteInklOverfoert = 324500
                        , yearlyTSGevinstNytteInklOverfoert = 28312.5216
                        , yearlyEksterneEffekterNytteInklOverfoert = 1528.2586
                        , yearlyNytteInklOverfoertSum = 1244321.2597
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -8877059.0867
                        , kostUtenSkyggepris = -8877059.0867
                        , skyggepris = -1775411.8173
                        , nytteInklOverfoert = 30383782.5336
                        , nettoNytteInklOverfoert = 19731311.6295
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

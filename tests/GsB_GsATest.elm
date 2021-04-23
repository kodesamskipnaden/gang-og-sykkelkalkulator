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


sykkelGsBState =
    { basicGsBTestState
        | sykkelturerPerYear = Just 5.0e4 |> formattedValue
        , gangturerPerYear = Just 0 |> formattedValue
    }


suite : Test
suite =
    describe "GsB_GsA"
        [ describe "Sykkelvei"
            [ let
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
              describe "MiddelsTilHøy Storby"
                [ tiltakSuite (createCheckWithState state) expectedRecord ]
            , let
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
              describe "LavTilMiddels Storby"
                [ tiltakSuite (createCheckWithState state) expectedRecord ]
            , skip <|
                let
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
                        { yearlySyklistNytteInklOverfoert = 541522.0903
                        , yearlyFotgjengerNytteInklOverfoert = 0
                        , yearlyTrafikantNytteInklOverfoert = 4939.1761
                        , yearlyHelsegevinstNytteInklOverfoert = 291894.8659
                        , yearlyTSGevinstNytteInklOverfoert = -21072.0181
                        , yearlyEksterneEffekterNytteInklOverfoert = 3978.4353
                        , yearlyNytteInklOverfoertSum = 821262.55
                        , nytteInklOverfoert = 20053553.3
                        , skyggepris = -1775411.8173
                        , driftOgVedlihKost = -8877059.0867
                        , investeringsKostInklRestverdi = 0
                        , kostUtenSkyggepris = -8877059.0867
                        , nettoNytteInklOverfoert = 9401082.4
                        }
                in
                describe "LavTilHøy Storby"
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
            ]
        , skip <|
            let
                fotgjengerGsBState =
                    { basicGsBTestState
                        | sykkelturerPerYear = Just 0 |> formattedValue
                        , gangturerPerYear = Just 5.0e4 |> formattedValue
                    }
            in
            describe "Gangvei"
                [ let
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
                        , yearlyFotgjengerNytteInklOverfoert = 864623.7934
                        , yearlyTrafikantNytteInklOverfoert = 449.3007
                        , yearlyHelsegevinstNytteInklOverfoert = 306745.6573
                        , yearlyTSGevinstNytteInklOverfoert = 74441.9495
                        , yearlyEksterneEffekterNytteInklOverfoert = 1131.0371
                        , yearlyNytteInklOverfoertSum = 1247391.738
                        , nytteInklOverfoert = 30458757.3388
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -8877059.0867
                        , kostUtenSkyggepris = -8877059.0867
                        , skyggepris = -1775411.8173
                        , nettoNytteInklOverfoert = 19806286.4347
                        }
                  in
                  describe "Liten by LavTilHøy"
                    [ tiltakSuite (createCheckWithState state) expectedRecord ]
                , let
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
                        , yearlyFotgjengerNytteInklOverfoert = 864623.7934
                        , yearlyTrafikantNytteInklOverfoert = 0
                        , yearlyHelsegevinstNytteInklOverfoert = 306745.6573
                        , yearlyTSGevinstNytteInklOverfoert = 74524.3951
                        , yearlyEksterneEffekterNytteInklOverfoert = 422.1363
                        , yearlyNytteInklOverfoertSum = 1246315.9822
                        , nytteInklOverfoert = 30432489.5798
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -8877059.0867
                        , kostUtenSkyggepris = -8877059.0867
                        , skyggepris = -1775411.8173
                        , nettoNytteInklOverfoert = 19780018.6757
                        }
                  in
                  describe "Spredtbygd LavTilHøy"
                    [ tiltakSuite (createCheckWithState state) expectedRecord ]
                , let
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
                        , yearlyFotgjengerNytteInklOverfoert = 864623.7934
                        , yearlyTrafikantNytteInklOverfoert = 1975.6705
                        , yearlyHelsegevinstNytteInklOverfoert = 306745.6575
                        , yearlyTSGevinstNytteInklOverfoert = 74363.3766
                        , yearlyEksterneEffekterNytteInklOverfoert = 1591.3741
                        , yearlyNytteInklOverfoertSum = 1249299.8719
                        , investeringsKostInklRestverdi = 0
                        , driftOgVedlihKost = -8877059.0867
                        , kostUtenSkyggepris = -8877059.0867
                        , skyggepris = -1775411.8173
                        , nytteInklOverfoert = 30505350.07
                        , nettoNytteInklOverfoert = 19852879.1659
                        }
                  in
                  describe "Storby LavTilHøy"
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
                ]
        ]

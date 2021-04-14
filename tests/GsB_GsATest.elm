module GsB_GsATest exposing (..)

import BasicState exposing (..)
import BasicTiltak
import Expect exposing (FloatingPointTolerance(..))
import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (Tiltak(..), analyse, sendTo)
import Tiltak.GsB_GsA as GsB_GsA exposing (tiltak)
import TiltakAndGroupData


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


sykkelSuite : Test
sykkelSuite =
    let
        sykkelGsBState =
            { basicGsBTestState
                | sykkelturerPerYear = Just 5.0e4 |> formattedValue
                , gangturerPerYear = Just 0 |> formattedValue
            }
    in
    describe "GsB_GsA sykkelvei"
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
                { yearlySyklistNytteInklOverfoert = 126511.8852
                , yearlyFotgjengerNytteInklOverfoert = 0
                , yearlyTrafikantNytteInklOverfoert = 987.8352
                , yearlyHelsegevinstNytteInklOverfoert = 58378.9732
                , yearlyTSGevinstNytteInklOverfoert = -2600.9708
                , yearlyEksterneEffekterNytteInklOverfoert = 795.6871
                , yearlyNytteInklOverfoertSum = 184073.4099
                , nytteInklOverfoert = 4494696.5364
                , investeringsKostInklRestverdi = 0
                , driftOgVedlihKost = -7192694.0292
                , kostUtenSkyggepris = -7192694.0292
                , skyggepris = -1438538.8058
                , nettoNytteInklOverfoert = -4136536.2986
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
                { yearlySyklistNytteInklOverfoert = 410480.3968
                , yearlyFotgjengerNytteInklOverfoert = 0
                , yearlyTrafikantNytteInklOverfoert = 3951.3409
                , yearlyHelsegevinstNytteInklOverfoert = 233515.8927
                , yearlyTSGevinstNytteInklOverfoert = -18459.6045
                , yearlyEksterneEffekterNytteInklOverfoert = 3182.7482
                , yearlyNytteInklOverfoertSum = 632670.7742
                , nytteInklOverfoert = 15448527.5142
                , investeringsKostInklRestverdi = 0
                , driftOgVedlihKost = -1684365.0575
                , kostUtenSkyggepris = -1684365.0575
                , skyggepris = -336873.0115
                , nettoNytteInklOverfoert = 13427289.4452
                }
          in
          describe "LavTilMiddels Storby"
            [ tiltakSuite (createCheckWithState state) expectedRecord ]
        , let
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
                    BasicTiltak.yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (Expect.equal 2500)
            , test "overfoerteGangturer" <|
                \() ->
                    BasicTiltak.yearlyOverfoerteGangturer tiltak state |> checkMaybe (Expect.equal 0)
            , test
                "tidsbesparelseMinPerTurSyklende"
              <|
                \() ->
                    BasicTiltak.tidsbesparelseMinPerTurSyklende tiltak state
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


fotgjengerSuite : Test
fotgjengerSuite =
    let
        fotgjengerGsBState =
            { basicGsBTestState
                | sykkelturerPerYear = Just 0 |> formattedValue
                , gangturerPerYear = Just 5.0e4 |> formattedValue
            }
    in
    describe "GsB_GsA fotgjengervei"
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
                    BasicTiltak.yearlyOverfoerteGangturer tiltak state |> checkMaybe (Expect.equal 2500)
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

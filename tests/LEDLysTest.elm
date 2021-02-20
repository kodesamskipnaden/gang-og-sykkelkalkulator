module LEDLysTest exposing (..)

import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import TestSupport exposing (..)
import Tiltak exposing (TiltakAccessor, sendTo)
import Tiltak.LEDLys as LEDLys exposing (tiltak, yearlyOverfoerteSykkelturer)
import TiltakAndGroupData


sykkelSuite : Test
sykkelSuite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | ledLys =
                    { installationCost = Just 1.0e6 |> formattedValue
                    , yearlyMaintenance = Just 0 |> formattedValue
                    , sykkelturerPerYear = Just 1.5e4 |> formattedValue
                    , gangturerPerYear = Just 0 |> formattedValue
                    , lengdeSykkelveiKm = Just 1 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlySyklistNytte = 18433.75
            , yearlySyklistNytteInklOverfoert = 18894.59
            , yearlyTrafikantNytte = 0
            , yearlyTrafikantNytteInklOverfoert = 3133.55
            , yearlyHelsegevinstNytteInklOverfoert = 85500
            , yearlyTSGevinstNytte = 52633.67
            , yearlyTSGevinstNytteInklOverfoert = 53045.98
            , yearlyEksterneEffekterNytteInklOverfoert = 942.83
            , syklistNytte = 450114.51
            , fotgjengerNytte = 0
            , trafikantNytte = 0
            , nytte = 1735321.1
            , nytteInklOverfoert = 3943913.91
            , investeringsKostInklRestverdi = -1.0e6
            , driftOgVedlihKost = 0
            , kostUtenSkyggepris = -1.0e6
            , skyggepris = -2.0e5
            , nettoNytte = 535321.1
            , nettoNytteInklOverfoert = 2743913.91
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
    describe "LEDLys sykkelvei"
        [ tiltakSuite checkWithState expectedRecord
        , test "flupps" <|
            \() ->
                yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (closeTo 750 2)
        ]

gangOgSykkelSuite : Test
gangOgSykkelSuite =
    let
        initialState =
            TiltakAndGroupData.initialTiltakStates

        state =
            { initialState
                | ledLys =
                    { installationCost = Just 1.0e6 |> formattedValue
                    , yearlyMaintenance = Just 0 |> formattedValue
                    , sykkelturerPerYear = Just 1.5e4 |> formattedValue
                    , gangturerPerYear = Just 2e4 |> formattedValue
                    , lengdeSykkelveiKm = Just 1 |> formattedValue
                    , preferredToGraph = ""
                    }
            }

        expectedRecord =
            { yearlySyklistNytte = 18433.75
            , yearlySyklistNytteInklOverfoert = 18894.59
            , yearlyTrafikantNytte = 0
            , yearlyTrafikantNytteInklOverfoert = 3133.55 + 1671.23
            , yearlyHelsegevinstNytteInklOverfoert = 85500
            , yearlyTSGevinstNytte = 52633.67
            , yearlyTSGevinstNytteInklOverfoert = 53045.98
            , yearlyEksterneEffekterNytteInklOverfoert = 942.83
            , syklistNytte = 450114.51
            , fotgjengerNytte = 0
            , trafikantNytte = 0
            , nytte = 1735321.1 + 2729345.08
            , nytteInklOverfoert = 3943913.91
            , investeringsKostInklRestverdi = -1.0e6
            , driftOgVedlihKost = 0
            , kostUtenSkyggepris = -1.0e6
            , skyggepris = -2.0e5
            , nettoNytte = 535321.1
            , nettoNytteInklOverfoert = 2743913.91
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
    describe "LEDLys gang og sykkelvei"
        [ gangSykkelveiTiltakSuite checkWithState expectedRecord
        , test "flupps" <|
            \() ->
                yearlyOverfoerteSykkelturer tiltak state |> checkMaybe (closeTo 750 2)
        ]

gangSykkelveiTiltakSuite checkWithState expectedRecord =
    Test.concat
        [ describe "nytte calculcations"
            [ checkWithState
                "yearlySyklistNytte"
                .yearlySyklistNytte
                (closeTo expectedRecord.yearlySyklistNytte 2)
            , checkWithState
                "yearlySyklistNytteInklOverfoert"
                .yearlySyklistNytteInklOverfoert
                (closeTo expectedRecord.yearlySyklistNytteInklOverfoert 2)
            , checkWithState
                "yearlyTrafikantNytte"
                .yearlyTrafikantNytte
                (closeTo expectedRecord.yearlyTrafikantNytte 2)
            , checkWithState
                "yearlyTrafikantNytteInklOverfoert"
                .yearlyTrafikantNytteInklOverfoert
                (closeTo expectedRecord.yearlyTrafikantNytteInklOverfoert 2)
            , checkWithState
                "syklistNytte"
                .syklistNytte
                (closeTo expectedRecord.syklistNytte 2)
            , checkWithState
                "fotgjengerNytte"
                .fotgjengerNytte
                (closeTo expectedRecord.fotgjengerNytte 2)
            , checkWithState
                "trafikantNytte"
                .trafikantNytte
                (closeTo expectedRecord.trafikantNytte 2)
            , checkWithState
                "yearlyHelsegevinstNytteInklOverfoert"
                .yearlyHelsegevinstNytteInklOverfoert
                (closeTo expectedRecord.yearlyHelsegevinstNytteInklOverfoert 2)
            , checkWithState
                "yearlyTSGevinstNytte"
                .yearlyTSGevinstNytte
                (closeTo expectedRecord.yearlyTSGevinstNytte 2)
            , checkWithState
                "yearlyTSGevinstNytteInklOverfoert"
                .yearlyTSGevinstNytteInklOverfoert
                (closeTo expectedRecord.yearlyTSGevinstNytteInklOverfoert 2)
            , checkWithState
                "yearlyEksterneEffekterNytteInklOverfoert"
                .yearlyEksterneEffekterNytteInklOverfoert
                (closeTo expectedRecord.yearlyEksterneEffekterNytteInklOverfoert 2)
            ]
        , describe "kost calculations"
            [ checkWithState
                "investeringsKostInklRestverdi"
                .investeringsKostInklRestverdi
                (closeTo expectedRecord.investeringsKostInklRestverdi 2)
            , checkWithState
                "driftOgVedlihKost"
                .driftOgVedlihKost
                (closeTo expectedRecord.driftOgVedlihKost 2)
            , checkWithState
                "kostUtenSkyggepris"
                .kostUtenSkyggepris
                (closeTo expectedRecord.kostUtenSkyggepris 2)
            , checkWithState
                "skyggepris"
                .skyggepris
                (closeTo expectedRecord.skyggepris 2)
            ]
        , describe "nettonytte calculations"
            [ checkWithState
                "nettoNytte"
                .nettoNytte
                (closeTo expectedRecord.nettoNytte 2)
            ]
        ]

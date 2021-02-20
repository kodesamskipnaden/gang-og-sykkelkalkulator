module TestSupport exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, only, skip, test)
import Tiltak exposing (TiltakAccessor)
import TiltakStates exposing (TiltakStates)


closeTo : Float -> Int -> Float -> Expectation
closeTo expected precision actual =
    let
        epsilon =
            toFloat (10 ^ negate precision) / 2

        difference =
            abs (expected - actual)
    in
    if difference < epsilon then
        Expect.pass

    else
        toString actual
            ++ " is not near enough to "
            ++ toString expected
            ++ " using "
            ++ toString precision
            ++ " digits of precision"
            |> Expect.fail


checkMaybe : (a -> Expectation) -> Maybe a -> Expectation
checkMaybe expectation maybeValue =
    maybeValue
        |> Maybe.map expectation
        |> Maybe.withDefault (Expect.fail <| "Got nothing")


type alias ExpectedRecord =
    { syklistNytte : Float
    , trafikantNytte : Float
    , nytte : Float
    , nytteInklOverfoert : Float
    , driftOgVedlihKost : Float
    , investeringsKostInklRestverdi : Float
    , skyggepris : Float
    , kostUtenSkyggepris : Float
    , nettoNytte : Float
    , nettoNytteInklOverfoert : Float
    , yearlySyklistNytte : Float
    , yearlySyklistNytteInklOverfoert : Float
    , yearlyTrafikantNytte : Float
    , yearlyTrafikantNytteInklOverfoert : Float
    , yearlyTSGevinstNytte : Float
    , yearlyTSGevinstNytteInklOverfoert : Float
    , yearlyHelsegevinstNytteInklOverfoert : Float
    , yearlyEksterneEffekterNytteInklOverfoert : Float
    }


type alias CheckWithStateFunction =
    String -> TiltakAccessor (TiltakStates -> Maybe Float) -> (Float -> Expectation) -> Test


tiltakSuite : CheckWithStateFunction -> ExpectedRecord -> Test
tiltakSuite checkWithState expectedRecord =
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
            , checkWithState
                "nytte"
                .nytte
                (closeTo expectedRecord.nytte 2)
            , checkWithState
                "nytteInklOverfoert"
                .nytteInklOverfoert
                (closeTo expectedRecord.nytteInklOverfoert 2)
            , checkWithState
                "nettoNytteInklOverfoert"
                .nettoNytteInklOverfoert
                (closeTo expectedRecord.nettoNytteInklOverfoert 2)
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

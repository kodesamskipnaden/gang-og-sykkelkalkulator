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
    { driftOgVedlihKost : Float
    , investeringsKostInklRestverdi : Float
    , kostUtenSkyggepris : Float
    , nettoNytte : Float
    , nytte : Float
    , brukerNytte : Float
    , skyggepris : Float
    , trafikantNytte : Float
    , yearlyTSGevinstNytte : Float
    , yearlyBrukerNytte : Float
    , yearlyBrukerNytteInklOverfoert : Float
    , yearlyTrafikantNytte : Float
    , yearlyTrafikantNytteInklOverfoert : Float
    , yearlyHelsegevinstNytteInklOverfoert : Float
    , yearlyTSGevinstNytteInklOverfoert : Float
    , yearlyEksterneEffekterNytteInklOverfoert : Float
    , nytteInklOverfoert : Float
    , nettoNytteInklOverfoert : Float
    }


type alias CheckWithStateFunction =
    String -> TiltakAccessor (TiltakStates -> Maybe Float) -> (Float -> Expectation) -> Test


tiltakSuite : CheckWithStateFunction -> ExpectedRecord -> Test
tiltakSuite checkWithState expectedRecord =
    Test.concat
        [ describe "nytte calculcations"
            [ checkWithState
                "yearlyBrukerNytte"
                .yearlyBrukerNytte
                (closeTo expectedRecord.yearlyBrukerNytte 2)
            , checkWithState
                "brukerNytte"
                .brukerNytte
                (closeTo expectedRecord.brukerNytte 2)
            , checkWithState
                "yearlyTrafikantNytte"
                .yearlyTrafikantNytte
                (closeTo expectedRecord.yearlyTrafikantNytte 2)
            , checkWithState
                "trafikantNytte"
                .trafikantNytte
                (closeTo expectedRecord.trafikantNytte 2)
            , checkWithState
                "yearlyTSGevinstNytte"
                .yearlyTSGevinstNytte
                (closeTo expectedRecord.yearlyTSGevinstNytte 2)
            , checkWithState
                "nytte"
                .nytte
                (closeTo expectedRecord.nytte 2)
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

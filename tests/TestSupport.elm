module TestSupport exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
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
    { nytte : Float
    , nytteInklOverfoert : Float
    , driftOgVedlihKost : Float
    , investeringsKostInklRestverdi : Float
    , skyggepris : Float
    , kostUtenSkyggepris : Float
    , nettoNytte : Float
    , nettoNytteInklOverfoert : Float
    , yearlySyklistNytte : Float
    , yearlySyklistNytteInklOverfoert : Float
    , yearlyFotgjengerNytteInklOverfoert : Float
    , yearlyTrafikantNytte : Float
    , yearlyTrafikantNytteInklOverfoert : Float
    , yearlyTSGevinstNytte : Float
    , yearlyTSGevinstNytteInklOverfoert : Float
    , yearlyHelsegevinstNytteInklOverfoert : Float
    , yearlyEksterneEffekterNytteInklOverfoert : Float
    }


type alias CheckWithStateFunction =
    String -> TiltakAccessor (TiltakStates -> Maybe Float) -> (Float -> Expectation) -> Test



-- tiltakSuite : CheckWithStateFunction -> ExpectedRecord -> Test


tiltakSuite checkWithState expectedRecord =
    -- let expectWithin4decimals value =
    Test.concat
        [ describe "yearly nytte"
            [ checkWithState
                "yearlySyklistNytteInklOverfoert"
                .yearlySyklistNytteInklOverfoert
                (Expect.within (Absolute 0.0001) expectedRecord.yearlySyklistNytteInklOverfoert)
            , checkWithState
                "yearlyFotgjengerNytteInklOverfoert"
                .yearlyFotgjengerNytteInklOverfoert
                (Expect.within (Absolute 0.0001) expectedRecord.yearlyFotgjengerNytteInklOverfoert)
            , checkWithState
                "yearlyTrafikantNytteInklOverfoert"
                .yearlyTrafikantNytteInklOverfoert
                (Expect.within (Absolute 0.001) expectedRecord.yearlyTrafikantNytteInklOverfoert)
            , checkWithState
                "yearlyHelsegevinstNytteInklOverfoert"
                .yearlyHelsegevinstNytteInklOverfoert
                (Expect.within (Absolute 0.001) expectedRecord.yearlyHelsegevinstNytteInklOverfoert)
            , checkWithState
                "yearlyTSGevinstNytteInklOverfoert"
                .yearlyTSGevinstNytteInklOverfoert
                (Expect.within (Absolute 0.001) expectedRecord.yearlyTSGevinstNytteInklOverfoert)
            , checkWithState
                "yearlyEksterneEffekterNytteInklOverfoert"
                .yearlyEksterneEffekterNytteInklOverfoert
                (Expect.within (Absolute 0.0001) expectedRecord.yearlyEksterneEffekterNytteInklOverfoert)
            , checkWithState
                "yearlyNytteInklOverfoertSum"
                .yearlyNytteInklOverfoertSum
                (Expect.within (Absolute 0.001) expectedRecord.yearlyNytteInklOverfoertSum)
            ]
        , describe "nytte over analyseperioden"
            [ checkWithState
                "nytteInklOverfoert"
                .nytteInklOverfoert
                (Expect.within (Absolute 0.01) expectedRecord.nytteInklOverfoert)
            ]
        , describe "kost calculations"
            [ checkWithState
                "investeringsKostInklRestverdi"
                .investeringsKostInklRestverdi
                (Expect.within (Absolute 0.0001) expectedRecord.investeringsKostInklRestverdi)
            , checkWithState
                "driftOgVedlihKost"
                .driftOgVedlihKost
                (Expect.within (Absolute 0.0001) expectedRecord.driftOgVedlihKost)
            , checkWithState
                "kostUtenSkyggepris"
                .kostUtenSkyggepris
                (Expect.within (Absolute 0.0001) expectedRecord.kostUtenSkyggepris)
            , checkWithState
                "skyggepris"
                .skyggepris
                (Expect.within (Absolute 0.0001) expectedRecord.skyggepris)
            ]
        , describe "nettonytte calculations"
            [ checkWithState
                "nettoNytteInklOverfoert"
                .nettoNytteInklOverfoert
                (Expect.within (Absolute 0.01) expectedRecord.nettoNytteInklOverfoert)
            ]
        ]

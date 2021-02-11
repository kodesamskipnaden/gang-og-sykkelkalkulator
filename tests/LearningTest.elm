module LearningTest exposing (..)

import Expect
import GeneralForutsetninger exposing (afaktor, afaktorVekst)
import Test exposing (Test, describe, test)
import TestSupport


suite =
    describe "Underliggende kalkulasjoner"
        [ test "afaktorVekst" <|
            \() ->
                afaktorVekst
                    |> TestSupport.closeTo 24.418 3
        , test "afaktor" <|
            \() ->
                afaktor |> TestSupport.closeTo 19.7928 4
        ]

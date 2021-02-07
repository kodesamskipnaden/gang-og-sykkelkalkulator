module LearningTest exposing (..)

import Expect
import GeneralForutsetninger exposing (afaktor, afaktorVekst)
import Test exposing (Test, describe, test)


suite =
    describe "Underliggende kalkulasjoner"
        [ test "afaktorVekst" <|
            \() ->
                afaktorVekst
                    |> Expect.equal 24.418
        , test "afaktor" <|
            \() ->
                afaktor |> Expect.equal 19.7928
        ]

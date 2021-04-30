module NumberFormat exposing (..)

import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), Locale)


norwegianLocale : Locale
norwegianLocale =
    let
        base =
            FormatNumber.Locales.spanishLocale
    in
    { base
        | decimals = Exact 3
        , thousandSeparator = " "
        , negativePrefix = "-"
    }


pretty : Float -> String
pretty value =
    prettyWithDecimals (Exact 0) value


prettyWithDecimals decimals value =
    FormatNumber.format { norwegianLocale | decimals = decimals } value


prettyTwoDecimals : Float -> String
prettyTwoDecimals value =
    prettyWithDecimals (Exact 2) value


maybePretty : Maybe Float -> String
maybePretty maybeValue =
    maybeValue
        |> Maybe.map pretty
        |> Maybe.withDefault "Ugyldig kalkulasjon"


maybePrettyTwoDecimals : Maybe Float -> String
maybePrettyTwoDecimals maybeValue =
    maybeValue
        |> Maybe.map prettyTwoDecimals
        |> Maybe.withDefault "Ugyldig kalkulasjon"

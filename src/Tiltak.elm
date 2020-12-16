module Tiltak exposing (..)

import Field exposing (Field)
import Focus exposing (Focus)
import TiltakStates exposing (TiltakStates)


type alias AnalyseData =
    { passasjerNytte : Maybe Float
    , kostUtenSkyggepris : Maybe Float
    , analysePeriode : Float
    , isProfitable : Maybe Bool
    , trafikantNytte : Maybe Float
    , nytte : Maybe Float
    , skyggepris : Maybe Float
    , nettoNytte : Maybe Float
    , nettoNyttePerBudsjettKrone : Maybe Float
    }


type Tiltak
    = Tiltak TiltakRecord


type alias StateCalculationMethod =
    Tiltak -> TiltakStates -> Maybe Float



{-
    Some invariants

   passasjerNytte + trafikantNytte + operatoerNytte == nytte

   nytte == (  yearlyPassasjerNytte
             + yearlyTrafikantNytte
             + yearlyOperatoerNytte) * afaktorVekst

   nettoNytte = nytte + kost -- kost is negative

-}


type alias TiltakRecord =
    { title : Tiltak -> String
    , fields : Tiltak -> List Field
    , passasjerNytte : StateCalculationMethod
    , trafikantNytte : StateCalculationMethod
    , nytte : StateCalculationMethod
    , skyggepris : StateCalculationMethod
    , skyggeprisHelper : Tiltak -> TiltakStates -> Float -> Maybe Float
    , kostUtenSkyggepris : StateCalculationMethod
    , nettoNytte : StateCalculationMethod
    , yearlyPassasjerNytte : StateCalculationMethod
    , yearlyTrafikantNytte : StateCalculationMethod
    , driftOgVedlihKost : StateCalculationMethod
    , investeringsKostInklRestverdi : StateCalculationMethod
    , graphId : Tiltak -> String
    , domId : Tiltak -> String
    , preferredField : Tiltak -> TiltakStates -> Maybe Field
    , preferredToGraphFocus : Focus TiltakStates String
    }


type alias TiltakAccessor a =
    TiltakRecord -> Tiltak -> a


sendTo : Tiltak -> TiltakAccessor a -> a
sendTo ((Tiltak object) as this) recordAccessor =
    recordAccessor object this


getAttr : Tiltak -> (TiltakRecord -> a) -> a
getAttr (Tiltak object) accessor =
    accessor object


bindTiltak : Tiltak -> a -> (TiltakAccessor (a -> b) -> b)
bindTiltak tiltak tiltakStates =
    \accessor -> sendTo tiltak accessor tiltakStates


analyse : Tiltak -> TiltakStates -> AnalyseData
analyse tiltak tiltakStates =
    let
        f =
            bindTiltak tiltak tiltakStates
    in
    { passasjerNytte = f .passasjerNytte
    , analysePeriode = 40
    , kostUtenSkyggepris = f .kostUtenSkyggepris
    , isProfitable = f .nettoNytte |> Maybe.map (\value -> value > 0)
    , trafikantNytte = f .trafikantNytte
    , nytte = f .nytte
    , skyggepris = f .skyggepris
    , nettoNytte = f .nettoNytte
    , nettoNyttePerBudsjettKrone =
        Maybe.map2
            (\nettoNytte kostUtenSkyggepris ->
                nettoNytte / negate kostUtenSkyggepris
            )
            (f .nettoNytte)
            (f .kostUtenSkyggepris)
    }

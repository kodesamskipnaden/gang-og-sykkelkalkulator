module Tiltak exposing (..)

import Field exposing (Field)
import Focus exposing (Focus)
import TiltakStates exposing (TiltakStates)


type alias AnalyseData =
    { syklistNytte : Maybe Float
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

   syklistNytte + trafikantNytte + <annenNytte> == nytte

   <enEllerAnnenNytte> == yearly<EnEllerAnnenNytte> * afaktorVekst

   nytte == (  yearlySyklistNytte
             + yearlyTrafikantNytte
             + yearly<AnnenNytte>) * afaktorVekst

   nettoNytte = nytte + kost -- kost is negative

-}


type alias TiltakRecord =
    { title : Tiltak -> String
    , fields : Tiltak -> List Field
    , syklistNytte : StateCalculationMethod
    , fotgjengerNytte : StateCalculationMethod
    , trafikantNytte : StateCalculationMethod
    , tsGevinstNytte : StateCalculationMethod
    , syklistNytteInklOverfoert : StateCalculationMethod
    , trafikantNytteInklOverfoert : StateCalculationMethod
    , helseGevinstNytteInklOverfoert : StateCalculationMethod
    , tsGevinstNytteInklOverfoert : StateCalculationMethod
    , eksterneEffekterNytteInklOverfoert : StateCalculationMethod
    , nytte : StateCalculationMethod
    , nytteInklOverfoert : StateCalculationMethod
    , driftOgVedlihKost : StateCalculationMethod
    , investeringsKostInklRestverdi : StateCalculationMethod
    , skyggepris : StateCalculationMethod
    , kostUtenSkyggepris : StateCalculationMethod
    , nettoNytte : StateCalculationMethod
    , nettoNytteInklOverfoert : StateCalculationMethod
    , yearlySyklistNytte : StateCalculationMethod
    , yearlyTrafikantNytte : StateCalculationMethod
    , yearlyTSGevinstNytte : StateCalculationMethod
    , yearlySyklistNytteInklOverfoert : StateCalculationMethod
    , yearlyTrafikantNytteInklOverfoert : StateCalculationMethod
    , yearlyTSGevinstNytteInklOverfoert : StateCalculationMethod
    , yearlyHelsegevinstNytteInklOverfoert : StateCalculationMethod
    , yearlyEksterneEffekterNytteInklOverfoert : StateCalculationMethod
    , skyggeprisHelper : StateCalculationMethod
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
    { syklistNytte = f .syklistNytteInklOverfoert
    , analysePeriode = 40
    , kostUtenSkyggepris = f .kostUtenSkyggepris
    , isProfitable = f .nettoNytte |> Maybe.map (\value -> value > 0)
    , trafikantNytte = f .trafikantNytteInklOverfoert
    , nytte = f .nytte
    , skyggepris = f .skyggepris
    , nettoNytte = f .nettoNytteInklOverfoert
    , nettoNyttePerBudsjettKrone =
        Maybe.map2
            (\nettoNytte kostUtenSkyggepris ->
                nettoNytte / negate kostUtenSkyggepris
            )
            (f .nettoNytteInklOverfoert)
            (f .kostUtenSkyggepris)
    }

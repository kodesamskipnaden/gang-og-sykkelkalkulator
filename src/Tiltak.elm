module Tiltak exposing (..)

import BasicState exposing (BasicState, Nivaa, Sted)
import Field exposing (Field)
import Focus exposing (Focus)
import TiltakStates


type alias TiltakStates =
    TiltakStates.TiltakStates


type alias AnalyseData =
    { syklistNytte : Maybe Float
    , fotgjengerNytte : Maybe Float
    , trafikantNytte : Maybe Float
    , helseGevinstNytte : Maybe Float
    , tsGevinstNytte : Maybe Float
    , eksterneEffekterNytte : Maybe Float
    , nytte : Maybe Float
    , kostUtenSkyggepris : Maybe Float
    , analysePeriode : Float
    , isProfitable : Maybe Bool
    , skyggepris : Maybe Float
    , nettoNytte : Maybe Float
    , nettoNyttePerBudsjettKrone : Maybe Float
    }


type Tiltak
    = Tiltak TiltakRecord


type alias StateCalculationMethod =
    Tiltak -> TiltakStates -> Maybe Float


type alias BrukerForutsetninger =
    { andelNyeBrukereFraBil : Float
    , andelNyeBrukereFraKollektivtransport : Float
    , andelNyeBrukereGenererte : Float
    , tsGevinstTiltak : Float
    , tsKostnad : Float
    , turerPerYearMaybe : Maybe Float
    , totalReiseDistanceKm : Float
    , helseGevinstBruker : Float
    , voTBruker : Float
    , tidsbesparelseMinPerTur : Maybe Float
    }


type alias BrukerforutsetningStateCalculationMethod =
    Tiltak -> TiltakStates -> BrukerForutsetninger -> Maybe Float



{-
    Some invariants

   syklistNytte + trafikantNytte + <annenNytte> == nytte

   <enEllerAnnenNytte> == yearly<EnEllerAnnenNytte> * afaktorVekst

   nytte == (  yearlySyklistNytte
             + yearlyTrafikantNytte
             + yearly<AnnenNytte>) * afaktorVekst

   nettoNytte = nytte + kost -- kost is negative

-}


type alias NivaaForutsetninger =
    { etterspoerselsEffekt : Float
    , tidsbesparelseGaaendeMinutterPerKilometer : Float
    , tidsbesparelseSyklendeMinutterPerKilometer : Float
    , tsGevinstGaaende : Float
    , tsGevinstSyklende : Float
    , wtp : Float
    , annuiserteDriftsKostnaderPerKm : Float
    }


type alias TiltakRecordHooks =
    { title : Tiltak -> String
    , fields : Tiltak -> List Field
    , basicState : TiltakStates -> BasicState
    , nivaaFocus : Focus TiltakStates Nivaa
    , stedFocus : Focus TiltakStates Sted
    , investeringsKostInklRestverdi : StateCalculationMethod
    , syklistForutsetninger : Tiltak -> TiltakStates -> BrukerForutsetninger
    , fotgjengerForutsetninger : Tiltak -> TiltakStates -> BrukerForutsetninger
    , nivaaForutsetninger : Tiltak -> TiltakStates -> NivaaForutsetninger
    }


type alias HooksPartial a specificState =
    { a | specificStateFocus : Focus TiltakStates { specificState | preferredToGraph : String } }


type alias Hooks specificState =
    HooksPartial TiltakRecordHooks specificState


type alias TiltakMethod =
    Tiltak -> TiltakStates


type alias TiltakRecordPartial a =
    { a
        | syklistNytteInklOverfoert : StateCalculationMethod
        , fotgjengerNytteInklOverfoert : StateCalculationMethod
        , trafikantNytteInklOverfoert : StateCalculationMethod
        , helseGevinstNytteInklOverfoert : StateCalculationMethod
        , tsGevinstNytteInklOverfoert : StateCalculationMethod
        , eksterneEffekterNytteInklOverfoert : StateCalculationMethod
        , nytteInklOverfoert : StateCalculationMethod
        , skyggepris : StateCalculationMethod
        , kostUtenSkyggepris : StateCalculationMethod
        , driftOgVedlihKost : StateCalculationMethod
        , nettoNytteInklOverfoert : StateCalculationMethod
        , yearlySyklistNytte : StateCalculationMethod
        , yearlyFotgjengerNytte : StateCalculationMethod
        , yearlyTSGevinstNytte : StateCalculationMethod
        , yearlySyklistNytteInklOverfoert : StateCalculationMethod
        , yearlyFotgjengerNytteInklOverfoert : StateCalculationMethod
        , yearlyTrafikantNytteInklOverfoert : StateCalculationMethod
        , yearlyTSGevinstNytteInklOverfoert : StateCalculationMethod
        , yearlyHelsegevinstNytteInklOverfoert : StateCalculationMethod
        , yearlyEksterneEffekterNytteInklOverfoert : StateCalculationMethod
        , yearlyNytteInklOverfoertSum : StateCalculationMethod
        , skyggeprisHelper : StateCalculationMethod
        , yearlyTSGevinstNytteOverfoert : StateCalculationMethod
        , yearlyTrafikantNytteInklOverfoertForBruker : BrukerforutsetningStateCalculationMethod
        , yearlyHelsegevinstNytteInklOverfoertForBruker : BrukerforutsetningStateCalculationMethod
        , yearlyTSGevinstNytteForBrukere : BrukerforutsetningStateCalculationMethod
        , yearlyTSGevinstNytteOverfoertForBrukere : BrukerforutsetningStateCalculationMethod
        , yearlyEksterneEffekterNytteInklOverfoertForBruker : BrukerforutsetningStateCalculationMethod
        , tidsbesparelseMinPerTurGaaende : StateCalculationMethod
        , tidsbesparelseMinPerTurSyklende : StateCalculationMethod
        , wtpNytte : BrukerforutsetningStateCalculationMethod
        , graphId : Tiltak -> String
        , domId : Tiltak -> String
        , preferredField : Tiltak -> TiltakStates -> Maybe Field
        , preferredToGraphFocus : Focus TiltakStates String
    }


type alias TiltakRecord =
    TiltakRecordPartial TiltakRecordHooks


type alias TiltakAccessor a =
    TiltakRecord -> Tiltak -> a


sendTo : Tiltak -> (TiltakRecord -> Tiltak -> a) -> a
sendTo ((Tiltak object) as this) recordAccessor =
    recordAccessor object this


getAttr : Tiltak -> (TiltakRecord -> a) -> a
getAttr (Tiltak object) accessor =
    accessor object


bindTiltak : Tiltak -> TiltakStates -> (TiltakRecord -> Tiltak -> TiltakStates -> a) -> a
bindTiltak tiltak tiltakStates =
    \accessor -> sendTo tiltak accessor tiltakStates


analyse : Tiltak -> TiltakStates -> AnalyseData
analyse tiltak tiltakStates =
    let
        f =
            bindTiltak tiltak tiltakStates
    in
    { analysePeriode = 40
    , isProfitable = f .nettoNytteInklOverfoert |> Maybe.map (\value -> value > 0)
    , syklistNytte = f .syklistNytteInklOverfoert
    , fotgjengerNytte = f .fotgjengerNytteInklOverfoert
    , trafikantNytte = f .trafikantNytteInklOverfoert
    , helseGevinstNytte = f .helseGevinstNytteInklOverfoert
    , tsGevinstNytte = f .tsGevinstNytteInklOverfoert
    , eksterneEffekterNytte = f .eksterneEffekterNytteInklOverfoert
    , nytte = f .nytteInklOverfoert
    , skyggepris = f .skyggepris
    , nettoNytte = f .nettoNytteInklOverfoert
    , kostUtenSkyggepris = f .kostUtenSkyggepris
    , nettoNyttePerBudsjettKrone =
        Maybe.map2
            (\nettoNytte kostUtenSkyggepris ->
                nettoNytte / negate kostUtenSkyggepris
            )
            (f .nettoNytteInklOverfoert)
            (f .kostUtenSkyggepris)
    }

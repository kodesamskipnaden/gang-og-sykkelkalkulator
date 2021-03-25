module Tiltak.GsB_GsA exposing (..)

import BasicTiltak
import Field exposing (Field, SimpleField)
import Focus exposing ((=>), Focus)
import FormattedValue
    exposing
        ( formattedValue
        , formattedValueDefault
        , gangturerPerYear
        , installationCost
        , lengdeVeiKm
        , sykkelturerPerYear
        , value
        , yearlyMaintenance
        )
import GeneralForutsetninger exposing (verdisettinger)
import SpecificStates exposing (GsB_GsAState)
import Tiltak exposing (Hooks, StateCalculationMethod, Tiltak(..), bindTiltak, sendTo)


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord tiltakRecordImplementation
    in
    Tiltak
        { basicTiltakRecord
            | yearlySyklistNytte = yearlySyklistNytte
            , yearlyFotgjengerNytte = yearlyFotgjengerNytte
            , yearlySyklistNytteInklOverfoert = yearlySyklistNytteInklOverfoert
            , yearlyFotgjengerNytteInklOverfoert = yearlyFotgjengerNytteInklOverfoert
            , yearlyTrafikantNytteInklOverfoertForBruker =
                \this ({ gsB_GsA } as state) brukerForutsetninger ->
                    Maybe.map2
                        (*)
                        gsB_GsA.oppetidPercent.value
                        (brukerForutsetninger |> basicTiltakRecord.yearlyTrafikantNytteInklOverfoertForBruker this state)
        }


tiltakRecordImplementation : Hooks GsB_GsAState
tiltakRecordImplementation =
    { title = \_ -> "GsB til GsA"
    , fields = \_ -> fields
    , specificStateFocus = specificState
    , investeringsKostInklRestverdi =
        \_ { gsB_GsA } ->
            BasicTiltak.investeringsKostInklRestverdi
                gsB_GsA
                levetid
    , driftOgVedlihKost =
        \_ { gsB_GsA } ->
            BasicTiltak.driftOgVedlihKost gsB_GsA
    , syklistForutsetninger = syklistForutsetninger
    , fotgjengerForutsetninger = fotgjengerForutsetninger
    , yearlyHelsegevinstNytteInklOverfoertForBruker = yearlyHelsegevinstNytteInklOverfoertForBruker
    , yearlyTSGevinstNytteForBrukere = yearlyTSGevinstNytteForBrukere
    , yearlyTSGevinstNytteOverfoertForBrukere = yearlyTSGevinstNytteOverfoertForBrukere
    , yearlyEksterneEffekterNytteInklOverfoertForBruker = yearlyEksterneEffekterNytteInklOverfoertForBruker
    }


initialState : GsB_GsAState
initialState =
    { installationCost = Just 0 |> formattedValue
    , yearlyMaintenance = formattedValueDefault
    , sykkelturerPerYear = Just 0 |> formattedValue
    , gangturerPerYear = Just 0 |> formattedValue
    , lengdeVeiKm = formattedValueDefault
    , oppetidPercent = Just 0.8 |> formattedValue
    , preferredToGraph = ""
    }


specificState :
    Focus
        { tiltakStates
            | gsB_GsA : GsB_GsAState
        }
        GsB_GsAState
specificState =
    Focus.create
        .gsB_GsA
        (\f tiltakStates ->
            { tiltakStates
                | gsB_GsA = f tiltakStates.gsB_GsA
            }
        )


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields


fieldDefinitions : List SimpleField
fieldDefinitions =
    let
        oppetidPercent =
            Focus.create
                .oppetidPercent
                (\f specificState ->
                    { specificState | oppetidPercent = f specificState.oppetidPercent }
                )
    in
    [ Field.installationCostSimpleField specificState
    , Field.yearlyMaintenanceSimpleField specificState
    , Field.lengdeVeiKmSimpleField specificState
    , Field.sykkelturerPerYearSimpleField specificState
    , Field.gangturerPerYearSimpleField specificState
    , { name = "oppetidPercent"
      , title = "Tiltakets oppetid, prosent"
      , placeholder = "Andel av aktuell tidsperiode hvor nivået GsA oppfylles (mindre enn 100% pga f.eks. at det tar tid fra nedbør skjer, til GsA-standard er gjenopprettet)"
      , focus = specificState => oppetidPercent
      , stepSize = 0.1
      }
    ]


levetid =
    40


tidsbesparelseMinutterPerTur =
    0.5


syklistForutsetninger { gsB_GsA } =
    let
        basic =
            BasicTiltak.basicSyklistForutsetninger gsB_GsA.sykkelturerPerYear.value
    in
    { basic
        | tsGevinstTiltak = verdisettinger.tsGevinstGsB_GsASyklende
        , etterspoerselsEffekt = verdisettinger.sykkelGsB_GsA
    }


fotgjengerForutsetninger { gsB_GsA } =
    let
        basic =
            BasicTiltak.basicFotgjengerForutsetninger gsB_GsA.gangturerPerYear.value
    in
    { basic
        | tsGevinstTiltak = verdisettinger.tsGevinstGsB_GsAGaaende
        , etterspoerselsEffekt = verdisettinger.fotgjengerGsB_GsA
    }


yearlySyklistNyttePerTur { gsB_GsA } antallTurer =
    Maybe.map2
        (\a b -> a * b * verdisettinger.reisetidSykkel * tidsbesparelseMinutterPerTur)
        gsB_GsA.oppetidPercent.value
        antallTurer


yearlySyklistNytte : StateCalculationMethod
yearlySyklistNytte this ({ gsB_GsA } as state) =
    yearlySyklistNyttePerTur state gsB_GsA.sykkelturerPerYear.value


yearlySyklistNytteInklOverfoert : StateCalculationMethod
yearlySyklistNytteInklOverfoert this state =
    let
        receiver =
            bindTiltak this state

        overfoertNytte =
            Maybe.map
                (\a -> a / 2)
                (yearlySyklistNyttePerTur state (syklistForutsetninger state |> BasicTiltak.yearlyOverfoerteTurer this))
    in
    Maybe.map2 (+) (receiver .yearlySyklistNytte) overfoertNytte


yearlyHelsegevinstNytteInklOverfoertForBruker this { gsB_GsA } brukerForutsetninger =
    Maybe.map4
        (\a b c d -> a * b * c * d)
        gsB_GsA.oppetidPercent.value
        (BasicTiltak.yearlyOverfoerteTurer this brukerForutsetninger)
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (Just brukerForutsetninger.helseTSGevinstBruker)


yearlyTSGevinstNytteForBrukere this { gsB_GsA } brukerForutsetninger =
    Maybe.map3
        (\lengde turerPerYear oppetidPercent ->
            min lengde brukerForutsetninger.totalReiseDistanceKm
                * turerPerYear
                * brukerForutsetninger.tsKostnad
                * brukerForutsetninger.tsGevinstTiltak
                * oppetidPercent
        )
        gsB_GsA.lengdeVeiKm.value
        brukerForutsetninger.turerPerYearMaybe
        gsB_GsA.oppetidPercent.value


yearlyTSGevinstNytteOverfoertForBrukere this { gsB_GsA } brukerForutsetninger =
    let
        nyeTurerFunc =
            BasicTiltak.nyeTurerFra this brukerForutsetninger

        beregning nyeTurerFraBil nyeTurerFraKollektiv nyeTurerFraGenererte =
            nyeTurerFraBil
                * (verdisettinger.tsKostnadBil
                    - brukerForutsetninger.tsKostnad
                  )
                + (nyeTurerFraKollektiv
                    * (verdisettinger.tsKostnadKollektiv
                        - brukerForutsetninger.tsKostnad
                      )
                  )
                - nyeTurerFraGenererte
                * brukerForutsetninger.tsKostnad
    in
    Maybe.map3 (\a b c -> a * b * c)
        gsB_GsA.oppetidPercent.value
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (Maybe.map3
            beregning
            (nyeTurerFunc .andelNyeBrukereFraBil)
            (nyeTurerFunc .andelNyeBrukereFraKollektivtransport)
            (nyeTurerFunc .andelNyeBrukereGenererte)
        )


yearlyEksterneEffekterNytteInklOverfoertForBruker this { gsB_GsA } brukerForutsetninger =
    let
        nyeTurer =
            BasicTiltak.nyeTurerFra this brukerForutsetninger

        overfoertFraBilNyttePerKm nyeTurerFraBil =
            nyeTurerFraBil
                * (verdisettinger.eksterneKostnaderBil
                    - brukerForutsetninger.eksterneKostnader
                  )

        overfoertFraKollektivNyttePerKm nyeTurerFraKollektiv =
            nyeTurerFraKollektiv
                * (verdisettinger.eksterneKostnaderKollektiv
                    - brukerForutsetninger.eksterneKostnader
                  )

        nytte nyeTurerFraBil nyeTurerFraKollektiv =
            brukerForutsetninger.totalReiseDistanceKm
                * (overfoertFraBilNyttePerKm nyeTurerFraBil + overfoertFraKollektivNyttePerKm nyeTurerFraKollektiv)
    in
    Maybe.map2 (*)
        gsB_GsA.oppetidPercent.value
        (Maybe.map2
            nytte
            (nyeTurer .andelNyeBrukereFraBil)
            (nyeTurer .andelNyeBrukereFraKollektivtransport)
        )


yearlyFotgjengerNyttePerTur antallTurer =
    antallTurer * verdisettinger.reisetidGange * tidsbesparelseMinutterPerTur


yearlyFotgjengerNytte this { gsB_GsA } =
    Maybe.map2
        (\a b -> a * yearlyFotgjengerNyttePerTur b)
        gsB_GsA.oppetidPercent.value
        gsB_GsA.gangturerPerYear.value


yearlyGangturer this state =
    fotgjengerForutsetninger state |> BasicTiltak.yearlyOverfoerteTurer this


yearlyFotgjengerNytteInklOverfoert this ({ gsB_GsA } as state) =
    let
        receiver =
            bindTiltak this state

        overfoertNytte =
            Maybe.map2
                (\antallTurer oppetidPercent ->
                    oppetidPercent * (yearlyFotgjengerNyttePerTur antallTurer / 2)
                )
                (fotgjengerForutsetninger state |> BasicTiltak.yearlyOverfoerteTurer this)
                gsB_GsA.oppetidPercent.value
    in
    Maybe.map2 (+)
        (receiver .yearlyFotgjengerNytte)
        overfoertNytte

module Tiltak.GsB_GsA exposing (..)

import BasicState exposing (Nivaa(..), Sted(..))
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
            | yearlyFotgjengerNytte = yearlyFotgjengerNytte
            , yearlyFotgjengerNytteInklOverfoert = yearlyFotgjengerNytteInklOverfoert
            , yearlyTrafikantNytteInklOverfoertForBruker =
                \this state brukerForutsetninger ->
                    Maybe.map2
                        (*)
                        state.gsB_GsA.oppetidPercent.value
                        (basicTiltakRecord.yearlyTrafikantNytteInklOverfoertForBruker
                            this
                            state
                            brukerForutsetninger
                        )
            , yearlyHelsegevinstNytteInklOverfoertForBruker =
                \this state brukerForutsetninger ->
                    Maybe.map2
                        (*)
                        state.gsB_GsA.oppetidPercent.value
                        (basicTiltakRecord.yearlyHelsegevinstNytteInklOverfoertForBruker
                            this
                            state
                            brukerForutsetninger
                        )
            , yearlyTSGevinstNytteForBrukere =
                \this state brukerForutsetninger ->
                    Maybe.map2
                        (*)
                        state.gsB_GsA.oppetidPercent.value
                        (basicTiltakRecord.yearlyTSGevinstNytteForBrukere
                            this
                            state
                            brukerForutsetninger
                        )
            , yearlyEksterneEffekterNytteInklOverfoertForBruker =
                \this state brukerForutsetninger ->
                    Maybe.map2
                        (*)
                        state.gsB_GsA.oppetidPercent.value
                        (basicTiltakRecord.yearlyEksterneEffekterNytteInklOverfoertForBruker
                            this
                            state
                            brukerForutsetninger
                        )
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
    , basicState =
        \{ gsB_GsA } ->
            { sykkelturerPerYear = gsB_GsA.sykkelturerPerYear
            , gangturerPerYear = gsB_GsA.gangturerPerYear
            , preferredToGraph = gsB_GsA.preferredToGraph
            , lengdeVeiKm = gsB_GsA.lengdeVeiKm
            , nivaa = gsB_GsA.nivaa
            , sted = gsB_GsA.sted
            }
    , yearlySyklistNyttePerTur = yearlySyklistNyttePerTur
    , syklistForutsetninger = syklistForutsetninger
    , fotgjengerForutsetninger = fotgjengerForutsetninger
    , yearlyTSGevinstNytteOverfoertForBrukere = yearlyTSGevinstNytteOverfoertForBrukere
    }


initialState : GsB_GsAState
initialState =
    { nivaa = LavTilHoey
    , sted = Storby
    , installationCost = Just 0 |> formattedValue
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


etterspoerselsEffektFotgjengerGsB_GsA nivaa =
    let
        lavTilHoey =
            5 / 100

        lavTilMiddels =
            4 / 100
    in
    case nivaa of
        LavTilHoey ->
            lavTilHoey

        LavTilMiddels ->
            4 / 100

        MiddelsTilHoey ->
            lavTilHoey - lavTilMiddels


syklistForutsetninger this state =
    let
        basic =
            BasicTiltak.basicSyklistForutsetninger this state
    in
    { basic
        | tsGevinstTiltak = verdisettinger.tsGevinstGsB_GsASyklende
        , etterspoerselsEffekt = verdisettinger.sykkelGsB_GsA
    }


fotgjengerForutsetninger ((Tiltak object) as this) state =
    let
        basic =
            BasicTiltak.basicFotgjengerForutsetninger this state

        basicState =
            object.basicState state
    in
    { basic
        | tsGevinstTiltak = verdisettinger.tsGevinstGsB_GsAGaaende
        , etterspoerselsEffekt =
            etterspoerselsEffektFotgjengerGsB_GsA basicState.nivaa

        -- verdisettinger.fotgjengerGsB_GsA
    }


yearlySyklistNyttePerTur { gsB_GsA } antallTurer =
    Maybe.map2
        (\a b -> a * b * verdisettinger.reisetidSykkel * tidsbesparelseMinutterPerTur)
        gsB_GsA.oppetidPercent.value
        antallTurer


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


yearlyFotgjengerNyttePerTur antallTurer =
    antallTurer * verdisettinger.reisetidGange * tidsbesparelseMinutterPerTur


yearlyFotgjengerNytte this { gsB_GsA } =
    Maybe.map2
        (\a b -> a * yearlyFotgjengerNyttePerTur b)
        gsB_GsA.oppetidPercent.value
        gsB_GsA.gangturerPerYear.value


yearlyGangturer this state =
    fotgjengerForutsetninger this state |> BasicTiltak.yearlyOverfoerteTurer this


yearlyFotgjengerNytteInklOverfoert this ({ gsB_GsA } as state) =
    let
        receiver =
            bindTiltak this state

        overfoertNytte =
            Maybe.map2
                (\antallTurer oppetidPercent ->
                    oppetidPercent * (yearlyFotgjengerNyttePerTur antallTurer / 2)
                )
                (fotgjengerForutsetninger this state |> BasicTiltak.yearlyOverfoerteTurer this)
                gsB_GsA.oppetidPercent.value
    in
    Maybe.map2 (+)
        (receiver .yearlyFotgjengerNytte)
        overfoertNytte

module Tiltak.LEDLys exposing (..)

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
import SpecificStates exposing (LEDLysState)
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
            , yearlyFotgjengerNytte = \_ _ -> Just 0
            , yearlySyklistNytteInklOverfoert = yearlySyklistNytteInklOverfoert
            , yearlyFotgjengerNytteInklOverfoert = \_ _ -> Just 0
        }


tiltakRecordImplementation : Hooks LEDLysState
tiltakRecordImplementation =
    { title = \_ -> "LED-lys for syklende og eller gående"
    , fields = \_ -> fields
    , specificStateFocus = specificState
    , investeringsKostInklRestverdi =
        \_ { ledLys } ->
            BasicTiltak.investeringsKostInklRestverdi
                ledLys
                levetid
    , driftOgVedlihKost =
        \_ { ledLys } ->
            BasicTiltak.driftOgVedlihKost ledLys
    , syklistForutsetninger = syklistForutsetninger
    , fotgjengerForutsetninger = fotgjengerForutsetninger
    , yearlyHelsegevinstNytteInklOverfoertForBruker = yearlyHelsegevinstNytteInklOverfoertForBruker
    , yearlyTSGevinstNytteForBrukere = yearlyTSGevinstNytteForBrukere
    , yearlyTSGevinstNytteOverfoertForBrukere = yearlyTSGevinstNytteOverfoertForBrukere
    , yearlyEksterneEffekterNytteInklOverfoertForBruker = yearlyEksterneEffekterNytteInklOverfoertForBruker
    }


initialState : LEDLysState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , sykkelturerPerYear = Just 0 |> formattedValue
    , gangturerPerYear = Just 0 |> formattedValue
    , lengdeVeiKm = formattedValueDefault
    , preferredToGraph = ""
    }


specificState :
    Focus
        { tiltakStates
            | ledLys : LEDLysState
        }
        LEDLysState
specificState =
    Focus.create
        .ledLys
        (\f tiltakStates ->
            { tiltakStates
                | ledLys = f tiltakStates.ledLys
            }
        )


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields


fieldDefinitions : List SimpleField
fieldDefinitions =
    [ Field.installationCostSimpleField specificState
    , Field.yearlyMaintenanceSimpleField specificState
    , Field.lengdeVeiKmSimpleField specificState
    , Field.sykkelturerPerYearSimpleField specificState
    , Field.gangturerPerYearSimpleField specificState
    ]


levetid =
    40


tidsbesparelseMinutterPerTur =
    0.5


syklistForutsetninger { ledLys } =
    let
        basic =
            BasicTiltak.basicSyklistForutsetninger ledLys.sykkelturerPerYear.value
    in
    { basic
        | tsGevinstTiltak = verdisettinger.tsGevinstLEDLysSyklende
        , etterspoerselsEffekt = verdisettinger.sykkelBedreBelysningLED
    }


fotgjengerForutsetninger { ledLys } =
    let
        basic =
            BasicTiltak.basicFotgjengerForutsetninger ledLys.gangturerPerYear.value
    in
    { basic
        | tsGevinstTiltak = verdisettinger.tsGevinstLEDLysGaaende
        , etterspoerselsEffekt = verdisettinger.fotgjengerBedreBelysningLED
    }


yearlySyklistNyttePerTur _ antallTurer =
    Maybe.map
        (\a -> a * verdisettinger.reisetidSykkel * tidsbesparelseMinutterPerTur)
        antallTurer


yearlySyklistNytte : StateCalculationMethod
yearlySyklistNytte this ({ ledLys } as state) =
    yearlySyklistNyttePerTur state ledLys.sykkelturerPerYear.value


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


yearlyHelsegevinstNytteInklOverfoertForBruker this state brukerForutsetninger =
    Maybe.map3
        (\a b c -> a * b * c)
        (BasicTiltak.yearlyOverfoerteTurer this brukerForutsetninger)
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (Just brukerForutsetninger.helseTSGevinstBruker)


yearlyTSGevinstNytteForBrukere this { ledLys } brukerForutsetninger =
    Maybe.map2
        (\turerPerYear lengde ->
            min lengde brukerForutsetninger.totalReiseDistanceKm
                * turerPerYear
                * brukerForutsetninger.tsKostnad
                * brukerForutsetninger.tsGevinstTiltak
        )
        brukerForutsetninger.turerPerYearMaybe
        ledLys.lengdeVeiKm.value


yearlyTSGevinstNytteOverfoertForBrukere this state brukerForutsetninger =
    let
        nyeTurerFunc =
            BasicTiltak.nyeTurerFra this brukerForutsetninger

        beregning nyeTurerFraBil nyeTurerFraKollektiv nyeTurerFraGenererte =
            nyeTurerFraBil
                * (verdisettinger.tsKostnadBil
                    - brukerForutsetninger.tsKostnad
                    * (1 - brukerForutsetninger.tsGevinstTiltak)
                  )
                + nyeTurerFraKollektiv
                * (verdisettinger.tsKostnadKollektiv
                    - brukerForutsetninger.tsKostnad
                    * (1 - brukerForutsetninger.tsGevinstTiltak)
                  )
                - nyeTurerFraGenererte
                * brukerForutsetninger.tsKostnad
                * (1 - brukerForutsetninger.tsGevinstTiltak)
    in
    Maybe.map2 (*)
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (Maybe.map3
            beregning
            (nyeTurerFunc .andelNyeBrukereFraBil)
            (nyeTurerFunc .andelNyeBrukereFraKollektivtransport)
            (nyeTurerFunc .andelNyeBrukereGenererte)
        )


yearlyEksterneEffekterNytteInklOverfoertForBruker this state brukerForutsetninger =
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
    Maybe.map2 nytte
        (nyeTurer .andelNyeBrukereFraBil)
        (nyeTurer .andelNyeBrukereFraKollektivtransport)

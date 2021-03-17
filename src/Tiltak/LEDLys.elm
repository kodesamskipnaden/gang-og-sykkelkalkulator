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
import Tiltak exposing (StateCalculationMethod, Tiltak(..), bindTiltak, sendTo)


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord specificState
    in
    Tiltak
        { basicTiltakRecord
            | title = \_ -> "LED-lys for syklende og eller gående"
            , fields = \_ -> fields
            , yearlySyklistNytte = yearlySyklistNytte
            , yearlyFotgjengerNytte = \_ _ -> Just 0
            , yearlyTSGevinstNytte = yearlyTSGevinstNytte
            , yearlySyklistNytteInklOverfoert = yearlySyklistNytteInklOverfoert
            , yearlyFotgjengerNytteInklOverfoert = \_ _ -> Just 0
            , yearlyTrafikantNytteInklOverfoert = yearlyTrafikantNytteInklOverfoert
            , yearlyHelsegevinstNytteInklOverfoert = yearlyHelsegevinstNytteInklOverfoert
            , yearlyTSGevinstNytteInklOverfoert = yearlyTSGevinstNytteInklOverfoert
            , yearlyEksterneEffekterNytteInklOverfoert = yearlyEksterneEffekterNytteInklOverfoert
            , investeringsKostInklRestverdi =
                \_ { ledLys } ->
                    BasicTiltak.investeringsKostInklRestverdi
                        ledLys
                        levetid
            , driftOgVedlihKost =
                \_ { ledLys } ->
                    BasicTiltak.driftOgVedlihKost ledLys
            , skyggepris =
                \this state ->
                    sendTo
                        this
                        .skyggeprisHelper
                        state
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
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , focus = specificState => installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Økte årlige drifts- og vedlikeholdskostnader"
      , placeholder = BasicTiltak.yearlyMaintenancePlaceholder
      , focus = specificState => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "lengdeVeiKm"
      , title = "Veilengde i kilometer"
      , placeholder = "Lengde vei (km)"
      , focus = specificState => lengdeVeiKm
      , stepSize = 5
      }
    , { name = "sykkelturerPerYear"
      , title = "Antall sykkelturer per år"
      , placeholder = "Turer på mørke tider som får nytte av tiltaket"
      , focus = specificState => sykkelturerPerYear
      , stepSize = 50
      }
    , { name = "gangturerPerYear"
      , title = "Antall gangturer per år"
      , placeholder = "Turer på mørke tider som får nytte av tiltaket"
      , focus = specificState => gangturerPerYear
      , stepSize = 50
      }
    ]


levetid =
    40


tidsbesparelseMinutterPerTur =
    0.5


syklistForutsetninger ledLys =
    let
        basic =
            BasicTiltak.basicSyklistForutsetninger ledLys.sykkelturerPerYear.value
    in
    { basic
        | tsGevinstTiltak = verdisettinger.tsGevinstLEDLysSyklende
        , etterspoerselsEffekt = verdisettinger.sykkelBedreBelysningLED
    }


fotgjengerForutsetninger ledLys =
    { andelNyeBrukereFraBil = verdisettinger.andelNyeFotgjengereFraBil
    , andelNyeBrukereFraKollektivtransport = verdisettinger.andelNyeFotgjengereFraKollektivtransport
    , andelNyeBrukereGenererte = verdisettinger.andelNyeFotgjengereGenererte
    , tsGevinstTiltak = verdisettinger.tsGevinstLEDLysGaaende
    , tsKostnad = verdisettinger.tsKostnadGange
    , eksterneKostnader = verdisettinger.eksterneKostnaderGange
    , turerPerYearMaybe = ledLys.gangturerPerYear.value
    , totalReiseDistanceKm = verdisettinger.fotgjengerTotalReiseDistanceKm
    , etterspoerselsEffekt = verdisettinger.fotgjengerBedreBelysningLED
    , helseTSGevinstBruker = verdisettinger.helseTSGevinstGange
    }


yearlySyklistNyttePerTur antallTurer =
    antallTurer * verdisettinger.reisetidSykkel * tidsbesparelseMinutterPerTur


yearlySyklistNytte : StateCalculationMethod
yearlySyklistNytte this ({ ledLys } as state) =
    Maybe.map yearlySyklistNyttePerTur ledLys.sykkelturerPerYear.value


yearlySyklistNytteInklOverfoert : StateCalculationMethod
yearlySyklistNytteInklOverfoert this ({ ledLys } as state) =
    let
        receiver =
            bindTiltak this state

        overfoertNytte =
            Maybe.map
                (\antallTurer ->
                    yearlySyklistNyttePerTur antallTurer / 2
                )
                (syklistForutsetninger ledLys |> yearlyOverfoerteTurer this)
    in
    Maybe.map2 (+) (receiver .yearlySyklistNytte) overfoertNytte


yearlyTrafikantNytteInklOverfoertForBruker this ({ ledLys } as state) brukerForutsetninger =
    let
        receiver =
            bindTiltak this state

        overfoertNytte =
            Maybe.map3 (\a b c -> a * b * c)
                (Just brukerForutsetninger.totalReiseDistanceKm)
                (nyeTurerFra this brukerForutsetninger .andelNyeBrukereFraBil)
                (Just verdisettinger.koekostnadBiler)
    in
    Maybe.map2 (+) (receiver .yearlyTrafikantNytte) overfoertNytte


yearlyTrafikantNytteInklOverfoert this ({ ledLys } as state) =
    Maybe.map2 (+)
        (syklistForutsetninger ledLys |> yearlyTrafikantNytteInklOverfoertForBruker this state)
        (fotgjengerForutsetninger ledLys |> yearlyTrafikantNytteInklOverfoertForBruker this state)


yearlyHelsegevinstNytteInklOverfoertForBruker this state brukerForutsetninger =
    Maybe.map3
        (\a b c -> a * b * c)
        (yearlyOverfoerteTurer this brukerForutsetninger)
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (Just brukerForutsetninger.helseTSGevinstBruker)


yearlyHelsegevinstNytteInklOverfoert this state =
    Maybe.map2 (+)
        (syklistForutsetninger state.ledLys
            |> yearlyHelsegevinstNytteInklOverfoertForBruker this state
        )
        (fotgjengerForutsetninger state.ledLys
            |> yearlyHelsegevinstNytteInklOverfoertForBruker this state
        )


yearlyTSGevinstNytte : StateCalculationMethod
yearlyTSGevinstNytte this ({ ledLys } as state) =
    Maybe.map2 (+)
        (syklistForutsetninger ledLys |> yearlyTSGevinstNytteForBrukere this state)
        (fotgjengerForutsetninger ledLys |> yearlyTSGevinstNytteForBrukere this state)


yearlyTSGevinstNytteForBrukere this ({ ledLys } as state) brukerForutsetninger =
    Maybe.map2
        (\turerPerYear lengde ->
            min lengde brukerForutsetninger.totalReiseDistanceKm
                * turerPerYear
                * brukerForutsetninger.tsKostnad
                * brukerForutsetninger.tsGevinstTiltak
        )
        brukerForutsetninger.turerPerYearMaybe
        ledLys.lengdeVeiKm.value


yearlyTSGevinstNytteInklOverfoert : StateCalculationMethod
yearlyTSGevinstNytteInklOverfoert this state =
    Maybe.map2 (+)
        (yearlyTSGevinstNytte this state)
        (yearlyTSGevinstNytteOverfoert this state)


yearlyTSGevinstNytteOverfoert this ({ ledLys } as state) =
    Maybe.map2 (+)
        (syklistForutsetninger ledLys |> yearlyTSGevinstNytteOverfoertForBrukere this state)
        (fotgjengerForutsetninger ledLys |> yearlyTSGevinstNytteOverfoertForBrukere this state)


yearlyTSGevinstNytteOverfoertForBrukere this state brukerForutsetninger =
    let
        nyeTurerFunc =
            nyeTurerFra this brukerForutsetninger

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


yearlyEksterneEffekterNytteInklOverfoertForBruker this ({ ledLys } as state) brukerForutsetninger =
    let
        nyeTurer =
            nyeTurerFra this brukerForutsetninger

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


yearlyEksterneEffekterNytteInklOverfoert this ({ ledLys } as state) =
    Maybe.map2 (+)
        (syklistForutsetninger ledLys |> yearlyEksterneEffekterNytteInklOverfoertForBruker this state)
        (fotgjengerForutsetninger ledLys |> yearlyEksterneEffekterNytteInklOverfoertForBruker this state)


yearlyOverfoerteSykkelturer : StateCalculationMethod
yearlyOverfoerteSykkelturer this state =
    syklistForutsetninger state.ledLys |> yearlyOverfoerteTurer this


yearlyOverfoerteTurer this brukerForutsetninger =
    let
        receiver =
            nyeTurerFra this brukerForutsetninger
    in
    Maybe.map3 (\a b c -> a + b + c)
        (receiver .andelNyeBrukereFraBil)
        (receiver .andelNyeBrukereFraKollektivtransport)
        (receiver .andelNyeBrukereGenererte)


nyeTurerFra this brukerForutsetninger andelsAccessor =
    Maybe.map3
        (\a b c -> a * b * c)
        brukerForutsetninger.turerPerYearMaybe
        (Just brukerForutsetninger.etterspoerselsEffekt)
        (andelsAccessor brukerForutsetninger |> Just)

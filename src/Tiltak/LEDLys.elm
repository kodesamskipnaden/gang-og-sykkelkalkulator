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
        , sykkelturerPerYear
        , value
        , yearlyMaintenance
        )
import GeneralForutsetninger exposing (verdisettinger)
import SpecificStates exposing (LEDLysState)
import Tiltak exposing (StateCalculationMethod, Tiltak(..), bindTiltak, sendTo)


ledTidsbesparelseMinutterPerTur : Float
ledTidsbesparelseMinutterPerTur =
    0.5


levetid : number
levetid =
    40


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
                \this ({ ledLys } as state) ->
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


fieldDefinitions : List SimpleField
fieldDefinitions =
    let
        lengdeVeiKm =
            Focus.create .lengdeVeiKm
                (\f specificState ->
                    { specificState
                        | lengdeVeiKm = f specificState.lengdeVeiKm
                    }
                )
    in
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


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields


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


syklistForutsetninger ledLys =
    { andelNyeBrukereFraBil = verdisettinger.andelNyeSyklisterFraBil
    , andelNyeBrukereFraKollektivtransport = verdisettinger.andelNyeSyklisterFraKollektivtransport
    , andelNyeBrukereGenererte = verdisettinger.andelNyeSyklisterGenererte
    , tsGevinstLEDLys = verdisettinger.tsGevinstLEDLysSyklende
    , tsKostnad = verdisettinger.tsKostnadSykkel
    , eksterneKostnader = verdisettinger.eksterneKostnaderSykkel
    , turerPerYearMaybe = ledLys.sykkelturerPerYear.value
    , totalReiseDistanceKm = verdisettinger.syklistTotalReiseDistanceKm
    , brukerBedreBelysningLED = verdisettinger.sykkelBedreBelysningLED
    , helseTSGevinstBruker = verdisettinger.helseTSGevinstSykkel
    }


fotgjengerForutsetninger ledLys =
    { andelNyeBrukereFraBil = verdisettinger.andelNyeFotgjengereFraBil
    , andelNyeBrukereFraKollektivtransport = verdisettinger.andelNyeFotgjengereFraKollektivtransport
    , andelNyeBrukereGenererte = verdisettinger.andelNyeFotgjengereGenererte
    , tsGevinstLEDLys = verdisettinger.tsGevinstLEDLysGaaende
    , tsKostnad = verdisettinger.tsKostnadGange
    , eksterneKostnader = verdisettinger.eksterneKostnaderGange
    , turerPerYearMaybe = ledLys.gangturerPerYear.value
    , totalReiseDistanceKm = verdisettinger.fotgjengerTotalReiseDistanceKm
    , brukerBedreBelysningLED = verdisettinger.fotgjengerBedreBelysningLED
    , helseTSGevinstBruker = verdisettinger.helseTSGevinstGange
    }


yearlySyklistNyttePerTur antallTurer =
    antallTurer * verdisettinger.reisetidSykkel * ledTidsbesparelseMinutterPerTur


yearlySyklistNytte : StateCalculationMethod
yearlySyklistNytte this ({ ledLys } as state) =
    Maybe.map yearlySyklistNyttePerTur ledLys.sykkelturerPerYear.value


yearlySyklistNytteInklOverfoert : StateCalculationMethod
yearlySyklistNytteInklOverfoert this ({ ledLys } as state) =
    let
        f =
            bindTiltak this state

        overfoertNytte =
            Maybe.map (\a -> yearlySyklistNyttePerTur a / 2)
                (syklistForutsetninger ledLys |> yearlyOverfoerteTurer this state)
    in
    Maybe.map2 (+) (f .yearlySyklistNytte) overfoertNytte


yearlyTrafikantNytteInklOverfoertForBruker this ({ ledLys } as state) brukerForutsetninger =
    let
        f =
            bindTiltak this state

        verdisettinger =
            GeneralForutsetninger.verdisettinger

        overfoertNytte =
            Maybe.map3 (\a b c -> a * b * c)
                (Just brukerForutsetninger.totalReiseDistanceKm)
                (nyeTurerFra this state brukerForutsetninger .andelNyeBrukereFraBil)
                (Just verdisettinger.koekostnadBiler)
    in
    Maybe.map2 (+) (f .yearlyTrafikantNytte) overfoertNytte


yearlyTrafikantNytteInklOverfoert this ({ ledLys } as state) =
    Maybe.map2 (+)
        (syklistForutsetninger ledLys |> yearlyTrafikantNytteInklOverfoertForBruker this state)
        (fotgjengerForutsetninger ledLys |> yearlyTrafikantNytteInklOverfoertForBruker this state)


yearlyHelsegevinstNytteInklOverfoertForBruker this state brukerForutsetninger =
    Maybe.map3
        (\a b c -> a * b * c)
        (yearlyOverfoerteTurer this state brukerForutsetninger)
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


yearlyTSGevinstNytteOverfoert this ({ ledLys } as state) =
    Maybe.map2 (+)
        (syklistForutsetninger ledLys |> yearlyTSGevinstNytteOverfoertForBrukere this state)
        (fotgjengerForutsetninger ledLys |> yearlyTSGevinstNytteOverfoertForBrukere this state)


yearlyTSGevinstNytteOverfoertForBrukere this state brukerForutsetninger =
    let
        nyeTurerFunc =
            nyeTurerFra this state brukerForutsetninger

        beregning nyeTurerFraBil nyeTurerFraKollektiv nyeTurerFraGenererte =
            nyeTurerFraBil
                * (verdisettinger.tsKostnadBil
                    - brukerForutsetninger.tsKostnad
                    * (1 - brukerForutsetninger.tsGevinstLEDLys)
                  )
                + nyeTurerFraKollektiv
                * (verdisettinger.tsKostnadKollektiv
                    - brukerForutsetninger.tsKostnad
                    * (1 - brukerForutsetninger.tsGevinstLEDLys)
                  )
                - nyeTurerFraGenererte
                * brukerForutsetninger.tsKostnad
                * (1 - brukerForutsetninger.tsGevinstLEDLys)
    in
    Maybe.map2 (*)
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (Maybe.map3
            beregning
            (nyeTurerFunc .andelNyeBrukereFraBil)
            (nyeTurerFunc .andelNyeBrukereFraKollektivtransport)
            (nyeTurerFunc .andelNyeBrukereGenererte)
        )


yearlyTSGevinstNytteForBrukere this ({ ledLys } as state) brukerForutsetninger =
    Maybe.map2
        (\turerPerYear lengde ->
            min lengde brukerForutsetninger.totalReiseDistanceKm
                * turerPerYear
                * brukerForutsetninger.tsKostnad
                * brukerForutsetninger.tsGevinstLEDLys
        )
        brukerForutsetninger.turerPerYearMaybe
        ledLys.lengdeVeiKm.value


yearlyTSGevinstNytte : StateCalculationMethod
yearlyTSGevinstNytte this ({ ledLys } as state) =
    Maybe.map2 (+)
        (syklistForutsetninger ledLys |> yearlyTSGevinstNytteForBrukere this state)
        (fotgjengerForutsetninger ledLys |> yearlyTSGevinstNytteForBrukere this state)


yearlyTSGevinstNytteInklOverfoert : StateCalculationMethod
yearlyTSGevinstNytteInklOverfoert this state =
    Maybe.map2 (+)
        (yearlyTSGevinstNytte this state)
        (yearlyTSGevinstNytteOverfoert this state)


yearlyEksterneEffekterNytteInklOverfoertForBruker this ({ ledLys } as state) brukerForutsetninger =
    let
        nyeTurer =
            nyeTurerFra this state brukerForutsetninger

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
    syklistForutsetninger state.ledLys |> yearlyOverfoerteTurer this state


yearlyOverfoerteTurer this state brukerForutsetninger =
    let
        sjmamsj =
            nyeTurerFra this state brukerForutsetninger
    in
    Maybe.map3 (\a b c -> a + b + c)
        (sjmamsj .andelNyeBrukereFraBil)
        (sjmamsj .andelNyeBrukereFraKollektivtransport)
        (sjmamsj .andelNyeBrukereGenererte)


nyeTurerFra this ({ ledLys } as state) brukerForutsetninger andelsAccessor =
    Maybe.map3
        (\a b c -> a * b * c)
        brukerForutsetninger.turerPerYearMaybe
        (Just brukerForutsetninger.brukerBedreBelysningLED)
        (andelsAccessor brukerForutsetninger |> Just)


nyeSykkelturerFra this ({ ledLys } as state) prosentAndel =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
    Maybe.map3
        (\a b c -> a * b * c)
        ledLys.sykkelturerPerYear.value
        (Just verdisettinger.sykkelBedreBelysningLED)
        (Just prosentAndel)

module Tiltak.LEDLys exposing (..)

import BasicTiltak
import Field exposing (Field, SimpleField)
import Focus exposing ((=>), Focus)
import FormattedValue
    exposing
        ( formattedValueDefault
        , installationCost
        , sykkelturerPerYear
        , value
        , yearlyMaintenance
        )
import GeneralForutsetninger exposing (verdisettinger)
import SpecificStates exposing (LEDLysState)
import Tiltak exposing (StateCalculationMethod, Tiltak(..), bindTiltak, sendTo)



-- nytteSkisse =
--     if lengdeVeiKm > syklistLEDTotalReiseDistanceKm then
--         syklistLEDTotalReiseDistanceKm
--             * antallSykkelturer
--             * tsKostnadSykkel
--             * tsGevinstLEDLysSyklende
--             + syklistLEDTotalReiseDistanceKm
--             * (nyeSykkelturerFraBil
--                 * (tsKostnadBil - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 + nyeSykkelturerFraKollektiv
--                 * (tsKostnadKollektiv - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 + nyeSykkelturerFraGange
--                 * (tsKostnadGange - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 - nyeSykkelturerFraGenererte
--                 * tsKostnadSykkel
--                 * (1 - tsGevinstLEDLysSyklende)
--               )
--     else
--         lengdeVeiKm
--             * antallSykkelturer
--             * tsKostnadSykkel
--             * tsGevinstLEDLysSyklende
--             + syklistLEDTotalReiseDistanceKm
--             * (nyeSykkelturerFraBil
--                 * (tsKostnadBil - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 + nyeSykkelturerFraKollektiv
--                 * (tsKostnadKollektiv - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 + nyeSykkelturerFraGange
--                 * (tsKostnadGange - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 - nyeSykkelturerFraGenererte
--                 * tsKostnadSykkel
--                 * (1 - tsGevinstLEDLysSyklende)
--               )


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


yearlySyklistNytte : StateCalculationMethod
yearlySyklistNytte this ({ ledLys } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        firstCalc sykkelturerPerYear =
            sykkelturerPerYear * verdisettinger.reisetidSykkel * ledTidsbesparelseMinutterPerTur
    in
    Maybe.map firstCalc ledLys.sykkelturerPerYear.value


yearlySyklistNytteInklOverfoert : StateCalculationMethod
yearlySyklistNytteInklOverfoert this ({ ledLys } as state) =
    let
        f =
            bindTiltak this state

        verdisettinger =
            GeneralForutsetninger.verdisettinger

        overfoertNytte =
            Maybe.map3 (\a b c -> (a * b * c) / 2)
                (syklistForutsetninger ledLys |> yearlyOverfoerteTurer this state)
                (Just ledTidsbesparelseMinutterPerTur)
                (Just verdisettinger.reisetidSykkel)
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
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
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


syklistForutsetninger ledLys =
    { andelNyeBrukereFraBil = verdisettinger.andelNyeSyklisterFraBil
    , andelNyeBrukereFraKollektivtransport = verdisettinger.andelNyeSyklisterFraKollektivtransport
    , andelNyeBrukereGenererte = verdisettinger.andelNyeSyklisterGenererte
    , tsGevinstLEDLys = verdisettinger.tsGevinstLEDLysSyklende
    , tsKostnad = verdisettinger.tsKostnadSykkel
    , turerPerYearMaybe = ledLys.sykkelturerPerYear.value
    , totalReiseDistanceKm = syklistLEDTotalReiseDistanceKm
    , brukerBedreBelysningLED = verdisettinger.sykkelBedreBelysningLED
    , helseTSGevinstBruker = verdisettinger.helseTSGevinstSykkel
    }


fotgjengerForutsetninger ledLys =
    { andelNyeBrukereFraBil = verdisettinger.andelNyeFotgjengereFraBil
    , andelNyeBrukereFraKollektivtransport = verdisettinger.andelNyeFotgjengereFraKollektivtransport
    , andelNyeBrukereGenererte = verdisettinger.andelNyeFotgjengereGenererte
    , tsGevinstLEDLys = verdisettinger.tsGevinstLEDLysGaaende
    , tsKostnad = verdisettinger.tsKostnadGange
    , turerPerYearMaybe = ledLys.gangturerPerYear.value
    , totalReiseDistanceKm = fotgjengerLEDTotalReiseDistanceKm
    , brukerBedreBelysningLED = verdisettinger.fotgjengerBedreBelysningLED
    , helseTSGevinstBruker = verdisettinger.helseTSGevinstGange
    }


yearlyTSGevinstNytteOverfoert this ({ ledLys } as state) =
    syklistForutsetninger ledLys |> yearlyTSGevinstNytteOverfoertForBrukere this state


yearlyTSGevinstNytteOverfoertForBrukere this ({ ledLys } as state) brukerForutsetninger =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

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
    syklistForutsetninger ledLys |> yearlyTSGevinstNytteForBrukere this state


yearlyTSGevinstNytteInklOverfoert : StateCalculationMethod
yearlyTSGevinstNytteInklOverfoert this ({ ledLys } as state) =
    Maybe.map2 (+)
        (yearlyTSGevinstNytte this state)
        (yearlyTSGevinstNytteOverfoert this state)


yearlyEksterneEffekterNytteInklOverfoert : StateCalculationMethod
yearlyEksterneEffekterNytteInklOverfoert this ({ ledLys } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        nytte nyeSykkelturerFraBil nyeSykkelturerFraKollektiv =
            syklistLEDTotalReiseDistanceKm * (nyeSykkelturerFraBil * (verdisettinger.eksterneKostnaderBil - verdisettinger.eksterneKostnaderSykkel) + nyeSykkelturerFraKollektiv * (verdisettinger.eksterneKostnaderKollektiv - verdisettinger.eksterneKostnaderSykkel))

        nyeSykkelturerFraBilMaybe =
            nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraBil

        nyeSykkelturerFraKollektivMaybe =
            nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraKollektivtransport
    in
    Maybe.map2 nytte nyeSykkelturerFraBilMaybe nyeSykkelturerFraKollektivMaybe


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


ledTidsbesparelseMinutterPerTur : Float
ledTidsbesparelseMinutterPerTur =
    0.5


syklistLEDTotalReiseDistanceKm =
    5


fotgjengerLEDTotalReiseDistanceKm =
    2


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
            | title = \_ -> "LED-lys for syklende"
            , fields = \_ -> fields
            , yearlySyklistNytte = yearlySyklistNytte
            , yearlyTSGevinstNytte = yearlyTSGevinstNytte
            , yearlySyklistNytteInklOverfoert = yearlySyklistNytteInklOverfoert
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
                        0
        }


initialState : LEDLysState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , sykkelturerPerYear = formattedValueDefault
    , gangturerPerYear = formattedValueDefault
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
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = BasicTiltak.yearlyMaintenancePlaceholder
      , focus = specificState => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "lengdeVeiKm"
      , title = "Sykkelvei lengde i kilometer"
      , placeholder = "Lengde sykkelvei (km)"
      , focus = specificState => lengdeVeiKm
      , stepSize = 5
      }
    , { name = "sykkelturerPerYear"
      , title = "Antall sykkelturer per år"
      , placeholder = "Turer på mørke tider som får nytte av tiltaket"
      , focus = specificState => sykkelturerPerYear
      , stepSize = 50
      }
    ]


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields

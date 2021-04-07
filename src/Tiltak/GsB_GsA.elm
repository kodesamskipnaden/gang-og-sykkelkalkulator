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
import GeneralForutsetninger exposing (verdisettinger, verifiserteVerdisettinger)
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
    , nivaaFocus = specificState => FormattedValue.nivaa
    , stedFocus = specificState => FormattedValue.sted
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



-- =60* (lengde gangvei/hastighet før - lengde gangvei hastighet etter)
-- = 60 * lengde * hastighetsdifferanse
-- =60* [(lengde gangvei/hastighet før) - (lengde gangvei/hastighet etter)]


nivaaForutsetninger nivaa =
    case nivaa of
        LavTilHoey ->
            { etterspoerselsEffektFotgjenger = 5 / 100
            , tsGevinstGaaende = verdisettinger.tsGevinstGsB_GsAGaaende
            , tidsbesparelseSyklendeMinutterPerKilometer = (17 - 13.1) * 60
            , tidsbesparelseGaaendeMinutterPerKilometer = (1 / 4.4 - 1 / 5.3) * 60
            , wtp = 3.16
            }

        LavTilMiddels ->
            -- { etterspoerselsEffektFotgjenger = 4 / 100 }
            Debug.crash "Not Implemented"

        MiddelsTilHoey ->
            -- { etterspoerselsEffektFotgjenger = 1 / 100 }
            Debug.crash "Not Implemented"


etterspoerselsEffektFotgjengerGsB_GsA nivaa =
    (nivaaForutsetninger nivaa).etterspoerselsEffektFotgjenger


tsGevinstGaaende nivaa =
    (nivaaForutsetninger nivaa).tsGevinstGaaende


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
        | tsGevinstTiltak = tsGevinstGaaende basicState.nivaa
        , etterspoerselsEffekt =
            etterspoerselsEffektFotgjengerGsB_GsA basicState.nivaa
    }


tidsbesparelseMinPerTurSyklende { gsB_GsA } =
    let
        tidsbesparelseMinPerKm =
            (nivaaForutsetninger gsB_GsA.nivaa).tidsbesparelseSyklendeMinutterPerKilometer
    in
    Maybe.map2 (*)
        gsB_GsA.lengdeVeiKm.value
        (Just tidsbesparelseMinPerKm)


tidsbesparelseMinPerTurGaaende { gsB_GsA } =
    let
        tidsbesparelseMinPerKm =
            (nivaaForutsetninger gsB_GsA.nivaa).tidsbesparelseGaaendeMinutterPerKilometer
    in
    Maybe.map2 (*)
        gsB_GsA.lengdeVeiKm.value
        (Just tidsbesparelseMinPerKm)


yearlySyklistNyttePerTur ({ gsB_GsA } as state) antallTurer =
    Maybe.map3
        (\a b c -> a * b * verdisettinger.reisetidSykkel * c)
        gsB_GsA.oppetidPercent.value
        antallTurer
        (tidsbesparelseMinPerTurSyklende state)


yearlyTSGevinstNytteOverfoertForBrukere ((Tiltak object) as this) state brukerForutsetninger =
    let
        nyeTurerFunc =
            BasicTiltak.nyeTurerFra this brukerForutsetninger

        tsKostnader =
            BasicTiltak.tsKostnader (object.basicState state).sted

        beregning nyeTurerFraBil nyeTurerFraKollektiv nyeTurerFraGenererte =
            nyeTurerFraBil
                * (tsKostnader.bil
                    - brukerForutsetninger.tsKostnad
                  )
                + (nyeTurerFraKollektiv
                    * (tsKostnader.kollektivtransport
                        - brukerForutsetninger.tsKostnad
                      )
                  )
                - nyeTurerFraGenererte
                * brukerForutsetninger.tsKostnad
    in
    Maybe.map3 (\a b c -> a * b * c)
        state.gsB_GsA.oppetidPercent.value
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (Maybe.map3
            beregning
            (nyeTurerFunc .andelNyeBrukereFraBil)
            (nyeTurerFunc .andelNyeBrukereFraKollektivtransport)
            (nyeTurerFunc .andelNyeBrukereGenererte)
        )


yearlyGangturer this state =
    fotgjengerForutsetninger this state |> BasicTiltak.yearlyOverfoerteTurer this


yearlyFotgjengerNyttePerTur ({ gsB_GsA } as state) antallTurer =
    Maybe.map2
        (\a b -> a * b * verifiserteVerdisettinger.voTGange)
        antallTurer
        (tidsbesparelseMinPerTurGaaende state)


yearlyFotgjengerNytte ((Tiltak object) as this) ({ gsB_GsA } as state) =
    yearlyFotgjengerNyttePerTur state (object.basicState state).gangturerPerYear.value



--
-- Min: laveste verdi av tiltakets lengde og total reiselengde
-- *      henter WTP for tiltaket basert på nivå
-- * (antall brukere i dag + 1/2 av nye brukere)
-- MIN($B$6;$B$12)*FINN.RAD(KJED.SAMMEN(C3;C4;C5);'Forutsetninger tiltak-nivå-sted'!$F$4:$AD$39;21;USANN)*($B$10+0,5*C23)
-- Min: laveste verdi av tiltakets lengde og total reiselengde
-- Finn.rad: henter WTP for tiltaket
-- ($B$10+0,5*C23)
-- antall brukere i dag + 1/2 av nye brukere


yearlyFotgjengerNytteInklOverfoert this ({ gsB_GsA } as state) =
    let
        receiver =
            bindTiltak this state

        overfoertNytte =
            Maybe.map
                (\fotgjengerNytte ->
                    fotgjengerNytte / 2
                )
                (yearlyFotgjengerNyttePerTur state (fotgjengerForutsetninger this state |> BasicTiltak.yearlyOverfoerteTurer this))
    in
    Maybe.map4 (\a b c x -> x * (a + b + c))
        (receiver .yearlyFotgjengerNytte)
        overfoertNytte
        (wtpNytte this state)
        gsB_GsA.oppetidPercent.value



-- yearlyFotgjengerNytteInklOverfoert this ({ gsB_GsA } as state) =


wtpNytte this state =
    let
        totalReiseDistanceKm =
            (fotgjengerForutsetninger this state).totalReiseDistanceKm

        distanseMaybe =
            Maybe.map
                (\lengdeGangVei -> min lengdeGangVei totalReiseDistanceKm)
                state.gsB_GsA.lengdeVeiKm.value

        wtp =
            (nivaaForutsetninger state.gsB_GsA.nivaa).wtp

        turerPlussMaybe =
            Maybe.map2
                (\antallTurer overfoerteTurer -> antallTurer + 0.5 * overfoerteTurer)
                state.gsB_GsA.gangturerPerYear.value
                (fotgjengerForutsetninger this state |> BasicTiltak.yearlyOverfoerteTurer this)
    in
    Maybe.map2 (\distanse turerPluss -> distanse * turerPluss * wtp)
        distanseMaybe
        turerPlussMaybe

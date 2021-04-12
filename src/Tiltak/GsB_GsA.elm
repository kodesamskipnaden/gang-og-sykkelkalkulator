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
        )
import GeneralForutsetninger exposing (verifiserteVerdisettinger)
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
            | yearlyFotgjengerNytteInklOverfoert = yearlyFotgjengerNytteInklOverfoert
            , yearlySyklistNytteInklOverfoert = yearlySyklistNytteInklOverfoert
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
    , driftOgVedlihKost = driftOgVedlihKost
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
    , yearlyFotgjengerNyttePerTur = yearlyFotgjengerNyttePerTur
    , syklistForutsetninger = syklistForutsetninger
    , fotgjengerForutsetninger = fotgjengerForutsetninger
    }


initialState : GsB_GsAState
initialState =
    { nivaa = LavTilHoey
    , sted = Storby
    , installationCost = Just 0 |> formattedValue
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
    let
        hastighet =
            { syklende =
                { lav = 13.1, middels = 15.7, hoey = 17 }
            , gaaende =
                { lav = 4.4, middels = 4.9, hoey = 5.3 }
            }

        tidsbesparelseMinutterPerKilometer fraKmt tilKmt =
            (1 / fraKmt - 1 / tilKmt) * 60
    in
    case nivaa of
        LavTilHoey ->
            { etterspoerselsEffekt = 5 / 100
            , tsGevinstGaaende = 0.454545455
            , tsGevinstSyklende = 0.014925373
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.syklende.lav
                    hastighet.syklende.hoey
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.gaaende.lav
                    hastighet.gaaende.hoey
            , wtp = 3.16
            , annuiserteDriftsKostnaderPerKm = 195000
            }

        LavTilMiddels ->
            { etterspoerselsEffekt = 4 / 100
            , tsGevinstGaaende = 0.151515152
            , tsGevinstSyklende = 0.004975124
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.syklende.lav
                    hastighet.syklende.middels
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.gaaende.lav
                    hastighet.gaaende.middels
            , wtp = 2.51
            , annuiserteDriftsKostnaderPerKm = 37000
            }

        MiddelsTilHoey ->
            { etterspoerselsEffekt = 1 / 100
            , tsGevinstGaaende = 0.357142857
            , tsGevinstSyklende = 0.01
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.syklende.middels
                    hastighet.syklende.hoey
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.gaaende.middels
                    hastighet.gaaende.hoey
            , wtp = 0.65
            , annuiserteDriftsKostnaderPerKm = 158000
            }


syklistForutsetninger ((Tiltak object) as this) state =
    let
        basic =
            BasicTiltak.basicSyklistForutsetninger this state

        basicState =
            object.basicState state
    in
    { basic
        | tsGevinstTiltak = (nivaaForutsetninger basicState.nivaa).tsGevinstSyklende

        -- etterspoerselsEffekt varierer IKKE mellom fotgjenger og syklist
        , etterspoerselsEffekt = (nivaaForutsetninger basicState.nivaa).etterspoerselsEffekt
    }


fotgjengerForutsetninger ((Tiltak object) as this) state =
    let
        basic =
            BasicTiltak.basicFotgjengerForutsetninger this state

        basicState =
            object.basicState state
    in
    { basic
        | tsGevinstTiltak = (nivaaForutsetninger basicState.nivaa).tsGevinstGaaende

        -- etterspoerselsEffekt varierer IKKE mellom fotgjenger og syklist
        , etterspoerselsEffekt = (nivaaForutsetninger basicState.nivaa).etterspoerselsEffekt
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


yearlyGangturer this state =
    fotgjengerForutsetninger this state |> BasicTiltak.yearlyOverfoerteTurer this


yearlySyklistNyttePerTur ({ gsB_GsA } as state) antallTurer =
    Maybe.map2
        (\a b -> a * b * verifiserteVerdisettinger.voTSykkel)
        antallTurer
        (tidsbesparelseMinPerTurSyklende state)


yearlyFotgjengerNyttePerTur ({ gsB_GsA } as state) antallTurer =
    Maybe.map2
        (\a b -> a * b * verifiserteVerdisettinger.voTGange)
        antallTurer
        (tidsbesparelseMinPerTurGaaende state)



-- eksperiment for å se hvordan man finne fellestrekk i tiltaksnytte
-- yearlyTiltakNytteInklOverfoertForBruker ((Tiltak object) as this) state brukerForutsetninger =
--     let
--         receiver =
--             bindTiltak this state
--         overfoertNytte =
--             Maybe.map
--                 (\a -> a / 2)
--                 (object.yearlyTiltakNyttePerTur state (brukerForutsetninger |> BasicTiltak.yearlyOverfoerteTurer this))
--     in
--     Maybe.map4 (\a b c x -> x * (a + b + c))
--         (receiver .yearlyTiltakNytteForBruker)
--         overfoertNytte
--         (wtpNytte this state brukerForutsetninger)
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

        boundFotgjengerForutsetninger =
            fotgjengerForutsetninger this state

        overfoertNytte =
            Maybe.map
                (\fotgjengerNytte ->
                    fotgjengerNytte / 2
                )
                (yearlyFotgjengerNyttePerTur state (boundFotgjengerForutsetninger |> BasicTiltak.yearlyOverfoerteTurer this))
    in
    Maybe.map4 (\a b c x -> x * (a + b + c))
        (receiver .yearlyFotgjengerNytte)
        overfoertNytte
        (wtpNytte this state boundFotgjengerForutsetninger)
        gsB_GsA.oppetidPercent.value


yearlySyklistNytteInklOverfoert this ({ gsB_GsA } as state) =
    let
        receiver =
            bindTiltak this state

        boundSyklistForutsetninger =
            syklistForutsetninger this state

        overfoertNytte =
            Maybe.map
                (\syklistNytte ->
                    syklistNytte / 2
                )
                (yearlySyklistNyttePerTur state (boundSyklistForutsetninger |> BasicTiltak.yearlyOverfoerteTurer this))
    in
    Maybe.map4 (\a b c x -> x * (a + b + c))
        (receiver .yearlySyklistNytte)
        overfoertNytte
        (wtpNytte this state boundSyklistForutsetninger)
        gsB_GsA.oppetidPercent.value



-- yearlyFotgjengerNytteInklOverfoert this ({ gsB_GsA } as state) =


wtpNytte this state brukerForutsetninger =
    let
        totalReiseDistanceKm =
            brukerForutsetninger.totalReiseDistanceKm

        turerPerYearMaybe =
            brukerForutsetninger.turerPerYearMaybe

        distanseMaybe =
            Maybe.map
                (\lengdeVei -> min lengdeVei totalReiseDistanceKm)
                state.gsB_GsA.lengdeVeiKm.value

        wtp =
            (nivaaForutsetninger state.gsB_GsA.nivaa).wtp

        turerPlussMaybe =
            Maybe.map2
                (\antallTurer overfoerteTurer -> antallTurer + 0.5 * overfoerteTurer)
                turerPerYearMaybe
                (brukerForutsetninger |> BasicTiltak.yearlyOverfoerteTurer this)
    in
    Maybe.map2 (\distanse turerPluss -> distanse * turerPluss * wtp)
        distanseMaybe
        turerPlussMaybe


yearlyDriftOgVedlikeholdsKostnad ((Tiltak object) as this) state =
    state.gsB_GsA.lengdeVeiKm.value
        |> Maybe.map (\lengde -> lengde * (nivaaForutsetninger state.gsB_GsA.nivaa).annuiserteDriftsKostnaderPerKm)


driftOgVedlihKost ((Tiltak object) as this) state =
    Maybe.map
        (\yearlyKostnad -> yearlyKostnad * GeneralForutsetninger.afaktor)
        (yearlyDriftOgVedlikeholdsKostnad this state)
        |> Maybe.map negate

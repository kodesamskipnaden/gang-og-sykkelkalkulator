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
import SpecificStates exposing (GsB_GsAState)
import Tiltak exposing (..)
import TiltakForutsetninger


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord tiltakRecordImplementation
    in
    Tiltak
        { basicTiltakRecord
            | yearlyFotgjengerNytteInklOverfoert =
                \this state ->
                    Maybe.map2 (*)
                        state.gsB_GsA.oppetidPercent.value
                        (basicTiltakRecord.yearlyFotgjengerNytteInklOverfoert this state)
            , yearlySyklistNytteInklOverfoert =
                \this state ->
                    Maybe.map2 (*)
                        state.gsB_GsA.oppetidPercent.value
                        (basicTiltakRecord.yearlySyklistNytteInklOverfoert this state)
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
    , basicState =
        \{ gsB_GsA } ->
            BasicState.createBasicState gsB_GsA
    , nivaaFocus = specificState => FormattedValue.nivaa
    , stedFocus = specificState => FormattedValue.sted
    , syklistForutsetninger = syklistForutsetninger
    , fotgjengerForutsetninger = fotgjengerForutsetninger
    , nivaaForutsetninger = nivaaForutsetninger
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


nivaaForutsetninger :
    Tiltak
    -> TiltakStates
    -> NivaaForutsetninger
nivaaForutsetninger ((Tiltak object) as this) state =
    let
        basicState =
            object.basicState state

        hastighet =
            { syklende =
                { lav = 13.1, middels = 15.7, hoey = 17 }
            , gaaende =
                { lav = 4.4, middels = 4.9, hoey = 5.3 }
            }

        tidsbesparelseMinutterPerKilometer fraKmt tilKmt =
            (1 / fraKmt - 1 / tilKmt) * 60
    in
    case basicState.nivaa of
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


syklistForutsetninger : Tiltak -> TiltakStates -> BrukerForutsetninger
syklistForutsetninger this state =
    let
        basic =
            TiltakForutsetninger.basicSyklistForutsetninger this state

        receiver =
            bindTiltak this state
    in
    { basic
        | tsGevinstTiltak = (receiver .nivaaForutsetninger).tsGevinstSyklende
    }


fotgjengerForutsetninger : Tiltak -> TiltakStates -> BrukerForutsetninger
fotgjengerForutsetninger ((Tiltak object) as this) state =
    let
        basic =
            TiltakForutsetninger.basicFotgjengerForutsetninger this state

        receiver =
            bindTiltak this state
    in
    { basic
        | tsGevinstTiltak = (receiver .nivaaForutsetninger).tsGevinstGaaende
    }


yearlyGangturer : Tiltak -> Tiltak.TiltakStates -> Maybe Float
yearlyGangturer this state =
    fotgjengerForutsetninger this state |> BasicTiltak.yearlyOverfoerteTurer this state



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

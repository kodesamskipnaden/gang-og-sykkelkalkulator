module Tiltak.Vegdekkestandard exposing (initialState, tiltak)

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
import SpecificStates exposing (VegdekkestandardState)
import Tiltak exposing (..)
import TiltakForutsetninger
import TiltakSupport


tiltak : Tiltak
tiltak =
    BasicTiltak.basicTiltakRecord tiltakRecordImplementation |> Tiltak


tiltakRecordImplementation : Hooks VegdekkestandardState
tiltakRecordImplementation =
    { title = \_ -> "Vegdekkestandard"
    , nivaaTitle =
        \nivaa ->
            case nivaa of
                LavTilMiddels ->
                    "Fra sjelden reasfaltering og lapping til middels hyppig (hvert 12. år) reasfaltering og tetting"

                LavTilHoey ->
                    "Fra sjelden reasfaltering og lapping til meget hyppig (hvert 6. år) reasfaltering (uten lapping/tetting)"

                MiddelsTilHoey ->
                    "Fra middels hyppig (hvert 12. år) reasfaltering og tetting til meget hyppig (hvert 6. år) reasfaltering (uten lapping/tetting)"
    , fields = \_ -> fields
    , specificStateFocus = specificState
    , investeringsKostInklRestverdi =
        \_ { vegdekkestandard } ->
            TiltakSupport.investeringsKostInklRestverdi
                vegdekkestandard
                levetid
    , basicState =
        \{ vegdekkestandard } ->
            BasicState.createBasicState vegdekkestandard
    , nivaaFocus = specificState => FormattedValue.nivaa
    , stedFocus = specificState => FormattedValue.sted
    , syklistForutsetninger = syklistForutsetninger
    , fotgjengerForutsetninger = fotgjengerForutsetninger
    , nivaaForutsetninger = nivaaForutsetninger
    }


initialState =
    { nivaa = LavTilHoey
    , sted = Storby
    , installationCost = formattedValueDefault
    , sykkelturerPerYear = Just 0 |> formattedValue
    , gangturerPerYear = Just 0 |> formattedValue
    , lengdeVeiKm = formattedValueDefault
    , preferredToGraph = ""
    }


specificState :
    Focus
        { tiltakStates
            | vegdekkestandard : VegdekkestandardState
        }
        VegdekkestandardState
specificState =
    Focus.create
        .vegdekkestandard
        (\f tiltakStates ->
            { tiltakStates
                | vegdekkestandard = f tiltakStates.vegdekkestandard
            }
        )


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields


fieldDefinitions : List SimpleField
fieldDefinitions =
    [ Field.installationCostSimpleField specificState
    , Field.lengdeVeiKmSimpleField specificState
    , Field.sykkelturerPerYearSimpleField specificState
    , Field.gangturerPerYearSimpleField specificState
    ]


levetid =
    40


nivaaForutsetninger : Tiltak -> TiltakStates -> NivaaForutsetninger
nivaaForutsetninger ((Tiltak object) as this) state =
    let
        basicState =
            object.basicState state

        hastighet =
            { syklende =
                { lav = 16.7, middels = 17, hoey = 17.3 }
            , gaaende =
                { lav = 4.9, middels = 5, hoey = 5.1 }
            }

        tidsbesparelseMinutterPerKilometer fraKmt tilKmt =
            (1 / fraKmt - 1 / tilKmt) * 60
    in
    case basicState.nivaa of
        LavTilHoey ->
            { annuiserteDriftsKostnaderPerKm = 23200
            , etterspoerselsEffekt = 2.5 / 100
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.lav hastighet.syklende.hoey
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.lav hastighet.gaaende.hoey
            , tsGevinstSyklende = 0.072519084
            , tsGevinstGaaende = 0.040505509
            , wtp = 1.59
            }

        LavTilMiddels ->
            { annuiserteDriftsKostnaderPerKm = 4500
            , etterspoerselsEffekt = 1.9 / 100
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.lav hastighet.syklende.middels
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.lav hastighet.gaaende.middels
            , tsGevinstSyklende = 0.022900763
            , tsGevinstGaaende = 0.018146468
            , wtp = 1.23
            }

        MiddelsTilHoey ->
            { annuiserteDriftsKostnaderPerKm = 18700
            , etterspoerselsEffekt = 0.6 / 100
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.middels hastighet.syklende.hoey
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.middels hastighet.gaaende.hoey
            , tsGevinstSyklende = 0.05078125
            , tsGevinstGaaende = 0.022772277
            , wtp = 0.36
            }


syklistForutsetninger : Tiltak -> TiltakStates -> BrukerForutsetninger
syklistForutsetninger this state =
    let
        receiver =
            bindTiltak this state

        basic =
            TiltakForutsetninger.basicSyklistForutsetninger this state
    in
    { basic
        | tsGevinstTiltak = (receiver .nivaaForutsetninger).tsGevinstSyklende
    }


fotgjengerForutsetninger : Tiltak -> TiltakStates -> BrukerForutsetninger
fotgjengerForutsetninger this state =
    let
        receiver =
            bindTiltak this state

        basic =
            TiltakForutsetninger.basicFotgjengerForutsetninger this state
    in
    { basic
        | tsGevinstTiltak = (receiver .nivaaForutsetninger).tsGevinstGaaende
    }

module Tiltak.Renhold exposing (initialState, tiltak)

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
import SpecificStates exposing (RenholdState)
import Tiltak exposing (..)
import TiltakForutsetninger
import TiltakSupport


tiltak : Tiltak
tiltak =
    BasicTiltak.basicTiltakRecord tiltakRecordImplementation |> Tiltak


tiltakRecordImplementation : Hooks RenholdState
tiltakRecordImplementation =
    { title = \_ -> "Renhold inkl. grus/strøsand"
    , fields = \_ -> fields
    , specificStateFocus = specificState
    , investeringsKostInklRestverdi =
        \_ { renhold } ->
            TiltakSupport.investeringsKostInklRestverdi
                renhold
                levetid
    , basicState =
        \{ renhold } ->
            BasicState.createBasicState renhold
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
            | renhold : RenholdState
        }
        RenholdState
specificState =
    Focus.create
        .renhold
        (\f tiltakStates ->
            { tiltakStates
                | renhold = f tiltakStates.renhold
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
            { annuiserteDriftsKostnaderPerKm = 76965
            , etterspoerselsEffekt = 1.8 / 100
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.lav hastighet.syklende.hoey
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.lav hastighet.gaaende.hoey
            , tsGevinstSyklende = 0.087242026
            , tsGevinstGaaende = 0.011566424
            , wtp = 1.16
            }

        LavTilMiddels ->
            { annuiserteDriftsKostnaderPerKm = 22470
            , etterspoerselsEffekt = 1.4 / 100
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.lav hastighet.syklende.middels
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.lav hastighet.gaaende.middels
            , tsGevinstSyklende = 0.041275797
            , tsGevinstGaaende = 0.007931262
            , wtp = 0.86
            }

        MiddelsTilHoey ->
            { annuiserteDriftsKostnaderPerKm = 54495
            , etterspoerselsEffekt = 0.4 / 100
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.middels hastighet.syklende.hoey
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.middels hastighet.gaaende.hoey
            , tsGevinstSyklende = 0.047945205
            , tsGevinstGaaende = 0.003664224
            , wtp = 0.3
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

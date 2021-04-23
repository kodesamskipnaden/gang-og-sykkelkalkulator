module Tiltak.LEDLys exposing (..)

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
import SpecificStates exposing (LEDLysState)
import Tiltak exposing (..)
import TiltakForutsetninger
import TiltakSupport


tiltak : Tiltak
tiltak =
    BasicTiltak.basicTiltakRecord tiltakRecordImplementation |> Tiltak


tiltakRecordImplementation : Hooks LEDLysState
tiltakRecordImplementation =
    { title = \_ -> "LED-lys for syklende og eller gående"
    , fields = \_ -> fields
    , specificStateFocus = specificState
    , investeringsKostInklRestverdi =
        \_ { ledLys } ->
            TiltakSupport.investeringsKostInklRestverdi
                ledLys
                levetid
    , basicState =
        \{ ledLys } ->
            BasicState.createBasicState ledLys
    , nivaaFocus = specificState => FormattedValue.nivaa
    , stedFocus = specificState => FormattedValue.sted
    , syklistForutsetninger = syklistForutsetninger
    , fotgjengerForutsetninger = fotgjengerForutsetninger
    , nivaaForutsetninger = nivaaForutsetninger
    }


nivaaForutsetninger : Tiltak -> TiltakStates -> NivaaForutsetninger
nivaaForutsetninger ((Tiltak object) as this) state =
    let
        basicState =
            object.basicState state

        hastighet =
            { syklende =
                { lav = 16.7, middels = 17.0, hoey = 17.3 }
            , gaaende =
                { lav = 4.9, middels = 5.0, hoey = 5.1 }
            }

        tidsbesparelseMinutterPerKilometer fraKmt tilKmt =
            (1 / fraKmt - 1 / tilKmt) * 60
    in
    case basicState.nivaa of
        LavTilHoey ->
            { annuiserteDriftsKostnaderPerKm = 99309
            , etterspoerselsEffekt = 4.3 / 100
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.lav hastighet.gaaende.hoey
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.lav hastighet.syklende.hoey
            , tsGevinstGaaende = 0.014473684
            , tsGevinstSyklende = 0.108928571
            , wtp = 2.71
            }

        LavTilMiddels ->
            { annuiserteDriftsKostnaderPerKm = 89316
            , etterspoerselsEffekt = 3.9 / 100
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.lav hastighet.gaaende.middels
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.lav hastighet.syklende.middels
            , tsGevinstGaaende = 0.013815789
            , tsGevinstSyklende = 0.108035714
            , wtp = 2.49
            }

        MiddelsTilHoey ->
            { annuiserteDriftsKostnaderPerKm = 9993
            , etterspoerselsEffekt =
                0.4 / 100
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.middels hastighet.gaaende.hoey
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.middels hastighet.syklende.hoey
            , tsGevinstGaaende = 0.000667111
            , tsGevinstSyklende = 0.001001001
            , wtp = 0.21
            }


initialState : LEDLysState
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
    , Field.lengdeVeiKmSimpleField specificState
    , Field.sykkelturerPerYearSimpleField specificState
    , Field.gangturerPerYearSimpleField specificState
    ]


levetid =
    40


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

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
import Tiltak exposing (Hooks, StateCalculationMethod, Tiltak(..), bindTiltak, sendTo)


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord tiltakRecordImplementation
    in
    Tiltak
        { basicTiltakRecord
            | yearlyFotgjengerNytte = \_ _ -> Just 0
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
    , basicState =
        \{ ledLys } ->
            { sykkelturerPerYear = ledLys.sykkelturerPerYear
            , gangturerPerYear = ledLys.gangturerPerYear
            , preferredToGraph = ledLys.preferredToGraph
            , lengdeVeiKm = ledLys.lengdeVeiKm
            , nivaa = ledLys.nivaa
            , sted = ledLys.sted
            }
    , nivaaFocus = specificState => FormattedValue.nivaa
    , stedFocus = specificState => FormattedValue.sted
    , yearlyFotgjengerNyttePerTur = \_ _ _ -> Nothing
    , syklistForutsetninger = syklistForutsetninger
    , fotgjengerForutsetninger = fotgjengerForutsetninger
    , nivaaForutsetninger = nivaaForutsetninger
    }


nivaaForutsetninger nivaa =
    let
        hastighet =
            { syklende =
                { lav = 16.7, middels = 17.0, hoey = 17.3 }
            , gaaende =
                { lav = 4.9, middels = 5.0, hoey = 5.1 }
            }

        tidsbesparelseMinutterPerKilometer fraKmt tilKmt =
            (1 / fraKmt - 1 / tilKmt) * 60
    in
    case nivaa of
        LavTilHoey ->
            { annuiserteDriftsKostnaderPerKm = 99309
            , etterspoerselsEffekt = 4.3 / 100
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.gaaende.lav hastighet.gaaende.hoey
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer hastighet.syklende.lav hastighet.syklende.hoey
            , tsGevinstGaaende = 0.1
            , tsGevinstSyklende = 0.136363636
            , wtp = 2.71
            }

        _ ->
            Debug.crash "Helvete"


initialState : LEDLysState
initialState =
    { nivaa = LavTilHoey
    , sted = Storby
    , installationCost = formattedValueDefault
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


syklistForutsetninger ((Tiltak object) as this) state =
    let
        basicState =
            object.basicState state

        receiver =
            bindTiltak this state

        basic =
            BasicTiltak.basicSyklistForutsetninger this state
    in
    { basic
        | tsGevinstTiltak = (object.nivaaForutsetninger basicState.nivaa).tsGevinstSyklende
    }


fotgjengerForutsetninger ((Tiltak object) as this) state =
    let
        basicState =
            object.basicState state

        basic =
            BasicTiltak.basicSyklistForutsetninger this state
    in
    { basic
        | tsGevinstTiltak = (object.nivaaForutsetninger basicState.nivaa).tsGevinstGaaende
    }

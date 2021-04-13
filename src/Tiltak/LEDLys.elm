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
import GeneralForutsetninger exposing (verifiserteVerdisettinger)
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
    case nivaa of
        LavTilHoey ->
            { annuiserteDriftsKostnaderPerKm = 0
            , etterspoerselsEffekt = 0
            , tidsbesparelseGaaendeMinutterPerKilometer = 0
            , tidsbesparelseSyklendeMinutterPerKilometer = 0
            , tsGevinstGaaende = 0
            , tsGevinstSyklende = 0
            , wtp = 0
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


syklistForutsetninger this state =
    let
        basic =
            BasicTiltak.basicSyklistForutsetninger this state
    in
    { basic
        | tsGevinstTiltak = 0 -- verdisettinger.tsGevinstLEDLysSyklende
    }


fotgjengerForutsetninger this state =
    let
        basic =
            BasicTiltak.basicFotgjengerForutsetninger this state
    in
    { basic
        | tsGevinstTiltak = 0 --- verdisettinger.tsGevinstLEDLysGaaende
    }

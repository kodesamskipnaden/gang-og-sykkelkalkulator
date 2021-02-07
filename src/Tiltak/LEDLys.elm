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
import GeneralForutsetninger
import SpecificStates exposing (LEDLysState)
import Tiltak exposing (StateCalculationMethod, Tiltak(..), sendTo)


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


yearlyBrukerNytte : StateCalculationMethod
yearlyBrukerNytte this ({ ledLys } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        firstCalc sykkelturerPerYear =
            sykkelturerPerYear * verdisettinger.reisetidSykkel * ledTidsbesparelseMinutterPerTur
    in
    Maybe.map firstCalc ledLys.sykkelturerPerYear.value


yearlyTSGevinstNytte : StateCalculationMethod
yearlyTSGevinstNytte this ({ ledLys } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        nytteKalkulasjon antallSykkelturer lengde =
            lengde * antallSykkelturer * verdisettinger.tsKostnadSykkel * verdisettinger.tsGevinstLEDLysSyklende
    in
    Maybe.map2
        (\turerPerYear lengde ->
            if lengde > ledTotalReiseDistanceKm then
                nytteKalkulasjon turerPerYear ledTotalReiseDistanceKm

            else
                nytteKalkulasjon turerPerYear lengde
        )
        ledLys.sykkelturerPerYear.value
        ledLys.lengdeSykkelveiKm.value


ledTidsbesparelseMinutterPerTur : Float
ledTidsbesparelseMinutterPerTur =
    0.5


ledTotalReiseDistanceKm =
    5


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
            , yearlyBrukerNytte = yearlyBrukerNytte
            , yearlyTSGevinstNytte = yearlyTSGevinstNytte
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
    , lengdeSykkelveiKm = formattedValueDefault
    , preferredToGraph = ""
    }


fieldDefinitions : List SimpleField
fieldDefinitions =
    let
        lengdeSykkelveiKm =
            Focus.create .lengdeSykkelveiKm
                (\f specificState ->
                    { specificState
                        | lengdeSykkelveiKm = f specificState.lengdeSykkelveiKm
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
    , { name = "lengdeSykkelveiKm"
      , title = "Sykkelvei lengde i kilometer"
      , placeholder = "Lengde sykkelvei (km)"
      , focus = specificState => lengdeSykkelveiKm
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

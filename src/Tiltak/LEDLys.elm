module Tiltak.LEDLys exposing (..)

import BasicTiltak
import Field exposing (Field, SimpleField)
import Focus exposing ((=>), Focus)
import FormattedValue
    exposing
        ( formattedValueDefault
        , installationCost
        , passengersPerYear
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


yearlyPassasjerNytte : StateCalculationMethod
yearlyPassasjerNytte this ({ ledLys } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        firstCalc passengersPerYear =
            passengersPerYear * verdisettinger.opphoyetHoldeplass

        first =
            Maybe.map firstCalc ledLys.passengersPerYear.value

        secondCalc beleggForbiPassasjererPerBuss yearlyTidsbesparelseMinutter =
            beleggForbiPassasjererPerBuss
                * yearlyTidsbesparelseMinutter
                * verdisettinger.reisetidKollektivTransport
    in
    Maybe.map2 (+) (Just 1) (Just 2)


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ ledLys } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
    Maybe.map (\minutter -> minutter * verdisettinger.operatoerKostnad) ledLys.yearlyTidsbesparelseMinutter.value


levetid : number
levetid =
    25


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
            , yearlyPassasjerNytte = yearlyPassasjerNytte
            , yearlyOperatoerNytte = yearlyOperatoerNytte
            , investeringsKostInklRestverdi =
                \_ { opphoeyetHoldeplass } ->
                    BasicTiltak.investeringsKostInklRestverdi
                        opphoeyetHoldeplass
                        levetid
            , driftOgVedlihKost =
                \_ { opphoeyetHoldeplass } ->
                    BasicTiltak.driftOgVedlihKost opphoeyetHoldeplass
            , skyggepris =
                \this ({ opphoeyetHoldeplass } as state) ->
                    sendTo
                        this
                        .skyggeprisHelper
                        state
                        opphoeyetHoldeplass.bompengeAndel
        }


initialState : LEDLysState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , bompengeAndel = 0
    , passengersPerYear = formattedValueDefault
    , lengdeSykkelveiKm = formattedValueDefault
    , yearlyTidsbesparelseMinutter = formattedValueDefault
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

        yearlyTidsbesparelseMinutter =
            Focus.create
                .yearlyTidsbesparelseMinutter
                (\f specificState ->
                    { specificState
                        | yearlyTidsbesparelseMinutter = f specificState.yearlyTidsbesparelseMinutter
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
    , { name = "passengersPerYear"
      , title = "Antall sykkelturer per år"
      , placeholder = "Turer på mørke tider som får nytte av tiltaket"
      , focus = specificState => passengersPerYear
      , stepSize = 50
      }
    , { name = "yearlyTidsbesparelseMinutter"
      , title = "Årlig tidsbesparelse"
      , placeholder = " Tidsbesparelse ved raskere på- og avstigning, minutter"
      , focus = specificState => yearlyTidsbesparelseMinutter
      , stepSize = 1000
      }
    ]


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields

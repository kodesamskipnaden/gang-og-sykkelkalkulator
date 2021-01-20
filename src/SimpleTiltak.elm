module SimpleTiltak exposing (..)

import BasicTiltak
import Field exposing (SimpleField)
import Focus exposing (..)
import FormattedValue
    exposing
        ( formattedValueDefault
        , installationCost
        , sykkelturerPerYear
        , value
        , yearlyMaintenance
        )
import SpecificStates exposing (SimpleCommonState)
import Tiltak exposing (StateCalculationMethod, Tiltak(..), bindTiltak, sendTo)
import TiltakStates exposing (TiltakStates)


type alias SimpleTiltak =
    { levetid : Float
    , nytteMultiplikator : Float
    , focus : Focus TiltakStates SimpleCommonState
    , title : String
    , isHoldeplassTiltak : Bool
    }


initialState : SimpleCommonState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , sykkelturerPerYear = formattedValueDefault
    , bompengeAndel = 0
    , preferredToGraph = ""
    }


fieldDefinitions : Focus TiltakStates SimpleCommonState -> Bool -> List SimpleField
fieldDefinitions tiltakFocus isHoldeplassTiltak =
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , focus = tiltakFocus => installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = BasicTiltak.yearlyMaintenancePlaceholder
      , focus = tiltakFocus => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "sykkelturerPerYear"
      , title = "Antall brukerer per år"
      , placeholder =
            case isHoldeplassTiltak of
                True ->
                    "Årlig antall påstigende brukerer på holdeplassen"

                False ->
                    "Årlig antall brukerer om bord"
      , focus = tiltakFocus => sykkelturerPerYear
      , stepSize = 50
      }
    ]


createTiltak : SimpleTiltak -> Tiltak
createTiltak simpleTiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord simpleTiltak.focus
    in
    Tiltak
        { basicTiltakRecord
            | title = \_ -> simpleTiltak.title
            , fields =
                \_ ->
                    Field.transformToFields
                        (fieldDefinitions
                            simpleTiltak.focus
                            simpleTiltak.isHoldeplassTiltak
                        )
            , skyggepris =
                \this state ->
                    sendTo this
                        .skyggeprisHelper
                        state
                        (Focus.get simpleTiltak.focus state).bompengeAndel
            , yearlyBrukerNytte =
                \_ state ->
                    state
                        |> Focus.get (simpleTiltak.focus => sykkelturerPerYear => value)
                        |> Maybe.map ((*) simpleTiltak.nytteMultiplikator)
            , driftOgVedlihKost =
                \_ state ->
                    state
                        |> Focus.get simpleTiltak.focus
                        |> BasicTiltak.driftOgVedlihKost
            , investeringsKostInklRestverdi =
                \_ state ->
                    BasicTiltak.investeringsKostInklRestverdi
                        (Focus.get simpleTiltak.focus state)
                        simpleTiltak.levetid
        }

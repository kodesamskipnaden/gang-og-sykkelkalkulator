module SuperSimpleTiltak exposing (..)

import BasicTiltak
import Field exposing (SimpleField)
import Focus exposing ((=>), Focus)
import FormattedValue
    exposing
        ( formattedValueDefault
        , sykkelturerPerYear
        , value
        , yearlyMaintenance
        )
import SpecificStates
    exposing
        ( SimpleCommonState
        , SuperSimpleCommonState
        )
import Tiltak exposing (StateCalculationMethod, Tiltak(..), bindTiltak, sendTo)
import TiltakStates exposing (TiltakStates)


type alias SuperSimpleTiltak a =
    { nytteMultiplikator : Float
    , focus : Focus a SuperSimpleCommonState
    , title : String
    }


initialState : SuperSimpleCommonState
initialState =
    { yearlyMaintenance = formattedValueDefault
    , sykkelturerPerYear = formattedValueDefault
    , bompengeAndel = 0
    , preferredToGraph = ""
    }


fieldDefinitions : Focus TiltakStates SuperSimpleCommonState -> List SimpleField
fieldDefinitions tiltakFocus =
    [ { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = BasicTiltak.yearlyMaintenancePlaceholder
      , focus = tiltakFocus => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "sykkelturerPerYear"
      , title = "Antall brukerer per år"
      , placeholder = "Påstigende brukerer per år"
      , focus = tiltakFocus => sykkelturerPerYear
      , stepSize = 50
      }
    ]


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
                        (fieldDefinitions simpleTiltak.focus)
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
                    BasicTiltak.driftOgVedlihKost <| Focus.get simpleTiltak.focus state
            , investeringsKostInklRestverdi =
                \_ state ->
                    Just 0
        }

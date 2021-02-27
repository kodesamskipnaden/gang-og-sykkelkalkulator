module Tiltak.GsB_GsA exposing (..)

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
import SpecificStates exposing (GsB_GsAState)
import Tiltak exposing (StateCalculationMethod, Tiltak(..), bindTiltak, sendTo)


specificState :
    Focus
        { tiltakStates
            | gsA_GsB : GsB_GsAState
        }
        GsB_GsAState
specificState =
    Focus.create
        .gsA_GsB
        (\f tiltakStates ->
            { tiltakStates
                | gsA_GsB = f tiltakStates.gsA_GsB
            }
        )


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
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , focus = specificState => installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Økte årlige drifts- og vedlikeholdskostnader"
      , placeholder = BasicTiltak.yearlyMaintenancePlaceholder
      , focus = specificState => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "lengdeVeiKm"
      , title = "Veilengde i kilometer"
      , placeholder = "Lengde vei (km)"
      , focus = specificState => lengdeVeiKm
      , stepSize = 5
      }
    , { name = "sykkelturerPerYear"
      , title = "Antall sykkelturer per år"
      , placeholder = "Turer på mørke tider som får nytte av tiltaket"
      , focus = specificState => sykkelturerPerYear
      , stepSize = 50
      }
    , { name = "gangturerPerYear"
      , title = "Antall gangturer per år"
      , placeholder = "Turer på mørke tider som får nytte av tiltaket"
      , focus = specificState => gangturerPerYear
      , stepSize = 50
      }
    , { name = "oppetidPercent"
      , title = "Tiltakets oppetid, prosent"
      , placeholder = "Andel av aktuell tidsperiode hvor nivået GsA oppfylles (mindre enn 100% pga f.eks. at det tar tid fra nedbør skjer, til GsA-standard er gjenopprettet)"
      , focus = specificState => oppetidPercent
      , stepSize = 1
      }
    ]


levetid =
    40


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord specificState
    in
    Tiltak
        { basicTiltakRecord
            | title = \_ -> "LED-lys for syklende og eller gående"
            , fields = \_ -> fields
            , yearlySyklistNytte = yearlySyklistNytte
            , yearlyTSGevinstNytte = yearlyTSGevinstNytte
            , yearlySyklistNytteInklOverfoert = yearlySyklistNytteInklOverfoert
            , yearlyTrafikantNytteInklOverfoert = yearlyTrafikantNytteInklOverfoert
            , yearlyHelsegevinstNytteInklOverfoert = yearlyHelsegevinstNytteInklOverfoert
            , yearlyTSGevinstNytteInklOverfoert = yearlyTSGevinstNytteInklOverfoert
            , yearlyEksterneEffekterNytteInklOverfoert = yearlyEksterneEffekterNytteInklOverfoert
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
        }

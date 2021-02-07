module Tiltak.OpphoeyetHoldeplass exposing (..)

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
import SpecificStates exposing (OpphoeyetHoldeplassState)
import Tiltak exposing (StateCalculationMethod, Tiltak(..), sendTo)


specificState :
    Focus
        { tiltakStates
            | opphoeyetHoldeplass : OpphoeyetHoldeplassState
        }
        OpphoeyetHoldeplassState
specificState =
    Focus.create
        .opphoeyetHoldeplass
        (\f tiltakStates ->
            { tiltakStates
                | opphoeyetHoldeplass = f tiltakStates.opphoeyetHoldeplass
            }
        )


yearlyBrukerNytte : StateCalculationMethod
yearlyBrukerNytte this ({ opphoeyetHoldeplass } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        firstCalc sykkelturerPerYear =
            sykkelturerPerYear * 3.14

        -- verdisettinger.opphoyetHoldeplass
        first =
            Maybe.map firstCalc opphoeyetHoldeplass.sykkelturerPerYear.value

        secondCalc beleggForbiPassasjererPerBuss yearlyTidsbesparelseMinutter =
            beleggForbiPassasjererPerBuss
                * yearlyTidsbesparelseMinutter
                * verdisettinger.reisetidKollektivTransport

        second =
            Maybe.map2
                secondCalc
                opphoeyetHoldeplass.beleggForbiPassasjererPerBuss.value
                opphoeyetHoldeplass.yearlyTidsbesparelseMinutter.value
    in
    Maybe.map2 (+) first second


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
            | title = \_ -> "Opphøyet holdeplass"
            , fields = \_ -> fields
            , yearlyBrukerNytte = yearlyBrukerNytte
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
                        0
        }


initialState : OpphoeyetHoldeplassState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , sykkelturerPerYear = formattedValueDefault
    , beleggForbiPassasjererPerBuss = formattedValueDefault
    , yearlyTidsbesparelseMinutter = formattedValueDefault
    , preferredToGraph = ""
    }


fieldDefinitions : List SimpleField
fieldDefinitions =
    let
        beleggForbiPassasjererPerBuss =
            Focus.create .beleggForbiPassasjererPerBuss
                (\f specificState ->
                    { specificState
                        | beleggForbiPassasjererPerBuss = f specificState.beleggForbiPassasjererPerBuss
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
    , { name = "sykkelturerPerYear"
      , title = "Antall av- og påstigende passasjerer på holdeplassen"
      , placeholder = "På- og avstigende passasjerer per år"
      , focus = specificState => sykkelturerPerYear
      , stepSize = 50
      }
    , { name = "beleggForbiPassasjererPerBuss"
      , title = "Gjennomsnittsbelegg forbi holdeplassen"
      , placeholder = "Passasjerer pr buss"
      , focus = specificState => beleggForbiPassasjererPerBuss
      , stepSize = 5
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

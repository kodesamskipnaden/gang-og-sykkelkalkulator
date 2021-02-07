module Tiltak.KollektivPrioriteringLyskryss exposing (..)

import BasicTiltak
import Field exposing (Field, SimpleField)
import Focus exposing (..)
import FormattedValue
    exposing
        ( formattedValueDefault
        , installationCost
        , sykkelturerPerYear
        , value
        , yearlyMaintenance
        )
import GeneralForutsetninger exposing (verdisettinger)
import SpecificStates exposing (KollektivPrioriteringLyskryssState)
import Tiltak exposing (StateCalculationMethod, Tiltak(..), sendTo)


levetid : number
levetid =
    15


tidsbesparelsePerAvgangSeconds : number
tidsbesparelsePerAvgangSeconds =
    20


yearlyBrukerNytte : StateCalculationMethod
yearlyBrukerNytte this tiltakStates =
    tiltakStates
        |> Focus.get (specificStateFocus => sykkelturerPerYear => value)
        |> Maybe.map
            (\sykkelturerPerYear ->
                (tidsbesparelsePerAvgangSeconds / 60)
                    * verdisettinger.reisetidKollektivTransport
                    * sykkelturerPerYear
            )


yearlyTrafikantNytte : StateCalculationMethod
yearlyTrafikantNytte this ({ kollektivPrioriteringLyskryss } as state) =
    Maybe.map3
        (\antallBilerForsinketPerAvgang antallPasserendeAvgangerPerYear forsinkelsePerBilSeconds ->
            antallBilerForsinketPerAvgang
                * antallPasserendeAvgangerPerYear
                * (negate forsinkelsePerBilSeconds / 60)
                * verdisettinger.reisetidBil
        )
        kollektivPrioriteringLyskryss.antallBilerForsinketPerAvgang.value
        kollektivPrioriteringLyskryss.antallPasserendeAvgangerPerYear.value
        kollektivPrioriteringLyskryss.forsinkelsePerBilSeconds.value


yearlyOperatoerNytte : StateCalculationMethod
yearlyOperatoerNytte this ({ kollektivPrioriteringLyskryss } as state) =
    let
        calculation passerendeAvganger =
            passerendeAvganger
                * (tidsbesparelsePerAvgangSeconds / 60)
                * 3.14

        -- verdisettinger.operatoerKostnad
    in
    kollektivPrioriteringLyskryss.antallPasserendeAvgangerPerYear.value
        |> Maybe.map calculation


investeringsKostInklRestverdi : StateCalculationMethod
investeringsKostInklRestverdi this ({ kollektivPrioriteringLyskryss } as state) =
    BasicTiltak.investeringsKostInklRestverdi
        kollektivPrioriteringLyskryss
        levetid


driftOgVedlihKost : StateCalculationMethod
driftOgVedlihKost this ({ kollektivPrioriteringLyskryss } as state) =
    BasicTiltak.driftOgVedlihKost kollektivPrioriteringLyskryss


skyggepris : StateCalculationMethod
skyggepris this ({ kollektivPrioriteringLyskryss } as state) =
    sendTo this .skyggeprisHelper state 0


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord specificStateFocus
    in
    Tiltak
        { basicTiltakRecord
            | title = \_ -> "Kollektivprioritering i lyskryss"
            , yearlyBrukerNytte = yearlyBrukerNytte
            , yearlyTrafikantNytte = yearlyTrafikantNytte
            , investeringsKostInklRestverdi = investeringsKostInklRestverdi
            , driftOgVedlihKost = driftOgVedlihKost
            , skyggepris = skyggepris
            , fields = \_ -> fields
        }


initialState : KollektivPrioriteringLyskryssState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , sykkelturerPerYear = formattedValueDefault
    , antallBilerForsinketPerAvgang = formattedValueDefault
    , forsinkelsePerBilSeconds = formattedValueDefault
    , antallPasserendeAvgangerPerYear = formattedValueDefault
    , preferredToGraph = ""
    }


specificStateFocus : Focus { b | kollektivPrioriteringLyskryss : a } a
specificStateFocus =
    Focus.create
        .kollektivPrioriteringLyskryss
        (\f tiltakStates ->
            { tiltakStates
                | kollektivPrioriteringLyskryss =
                    f tiltakStates.kollektivPrioriteringLyskryss
            }
        )


antallBilerForsinketPerAvgang : Focus { b | antallBilerForsinketPerAvgang : a } a
antallBilerForsinketPerAvgang =
    Focus.create
        .antallBilerForsinketPerAvgang
        (\f state ->
            { state
                | antallBilerForsinketPerAvgang = f state.antallBilerForsinketPerAvgang
            }
        )


forsinkelsePerBilSeconds : Focus { b | forsinkelsePerBilSeconds : a } a
forsinkelsePerBilSeconds =
    Focus.create
        .forsinkelsePerBilSeconds
        (\f state ->
            { state
                | forsinkelsePerBilSeconds = f state.forsinkelsePerBilSeconds
            }
        )


antallPasserendeAvgangerPerYear : Focus { b | antallPasserendeAvgangerPerYear : a } a
antallPasserendeAvgangerPerYear =
    Focus.create
        .antallPasserendeAvgangerPerYear
        (\f state ->
            { state
                | antallPasserendeAvgangerPerYear = f state.antallPasserendeAvgangerPerYear
            }
        )


fieldDefinitions : List SimpleField
fieldDefinitions =
    [ { name = "installationCost"
      , title = "Installasjonskostnad"
      , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
      , focus = specificStateFocus => installationCost
      , stepSize = 50000
      }
    , { name = "yearlyMaintenance"
      , title = "Årlige drifts- og vedlikeholdskostnader"
      , placeholder = BasicTiltak.yearlyMaintenancePlaceholder
      , focus = specificStateFocus => yearlyMaintenance
      , stepSize = 5000
      }
    , { name = "sykkelturerPerYear"
      , title = "Antall brukerer ombord per år"
      , placeholder = "Antall kollektivpassasjerer som omfattes av tiltaket"
      , focus = specificStateFocus => sykkelturerPerYear
      , stepSize = 50
      }
    , { name = "antallBilerForsinketPerAvgang"
      , title = "Antall forsinkete biler per avgang"
      , placeholder = "Antall biler som forsinkes per avgang"
      , focus = specificStateFocus => antallBilerForsinketPerAvgang
      , stepSize = 1
      }
    , { name = "forsinkelsePerBilSeconds"
      , title = "Sekunder forsinkelse per bil"
      , placeholder = "Når de blir forsinket, antall sekunder"
      , focus = specificStateFocus => forsinkelsePerBilSeconds
      , stepSize = 1
      }
    , { name = "antallPasserendeAvgangerPerYear"
      , title = "Avganger som passererer krysset"
      , placeholder = "Antall passerende avganger per år"
      , focus = specificStateFocus => antallPasserendeAvgangerPerYear
      , stepSize = 1000
      }
    ]


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields

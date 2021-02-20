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
import GeneralForutsetninger exposing (verdisettinger)
import SpecificStates exposing (LEDLysState)
import Tiltak exposing (StateCalculationMethod, Tiltak(..), bindTiltak, sendTo)



-- nytteSkisse =
--     if lengdeSykkelveiKm > syklistLEDTotalReiseDistanceKm then
--         syklistLEDTotalReiseDistanceKm
--             * antallSykkelturer
--             * tsKostnadSykkel
--             * tsGevinstLEDLysSyklende
--             + syklistLEDTotalReiseDistanceKm
--             * (nyeSykkelturerFraBil
--                 * (tsKostnadBil - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 + nyeSykkelturerFraKollektiv
--                 * (tsKostnadKollektiv - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 + nyeSykkelturerFraGange
--                 * (tsKostnadGange - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 - nyeSykkelturerFraGenererte
--                 * tsKostnadSykkel
--                 * (1 - tsGevinstLEDLysSyklende)
--               )
--     else
--         lengdeSykkelveiKm
--             * antallSykkelturer
--             * tsKostnadSykkel
--             * tsGevinstLEDLysSyklende
--             + syklistLEDTotalReiseDistanceKm
--             * (nyeSykkelturerFraBil
--                 * (tsKostnadBil - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 + nyeSykkelturerFraKollektiv
--                 * (tsKostnadKollektiv - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 + nyeSykkelturerFraGange
--                 * (tsKostnadGange - tsKostnadSykkel * (1 - tsGevinstLEDLysSyklende))
--                 - nyeSykkelturerFraGenererte
--                 * tsKostnadSykkel
--                 * (1 - tsGevinstLEDLysSyklende)
--               )


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


yearlySyklistNytte : StateCalculationMethod
yearlySyklistNytte this ({ ledLys } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        firstCalc sykkelturerPerYear =
            sykkelturerPerYear * verdisettinger.reisetidSykkel * ledTidsbesparelseMinutterPerTur
    in
    Maybe.map firstCalc ledLys.sykkelturerPerYear.value


yearlySyklistNytteInklOverfoert : StateCalculationMethod
yearlySyklistNytteInklOverfoert this ({ ledLys } as state) =
    let
        f =
            bindTiltak this state

        verdisettinger =
            GeneralForutsetninger.verdisettinger

        overfoertNytte =
            Maybe.map3 (\a b c -> (a * b * c) / 2)
                (yearlyOverfoerteSykkelturer this state)
                (Just ledTidsbesparelseMinutterPerTur)
                (Just verdisettinger.reisetidSykkel)
    in
    Maybe.map2 (+) (f .yearlySyklistNytte) overfoertNytte


syklistYearlyTrafikantNytteInklOverfoert this ({ ledLys } as state) =
    let
        f =
            bindTiltak this state

        verdisettinger =
            GeneralForutsetninger.verdisettinger

        overfoertNytte =
            Maybe.map3 (\a b c -> a * b * c)
                (Just syklistLEDTotalReiseDistanceKm)
                (nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraBil)
                (Just verdisettinger.koekostnadBiler)
    in
    Maybe.map2 (+) (f .yearlyTrafikantNytte) overfoertNytte


fotgjengerYearlyTrafikantNytteInklOverfoert this ({ ledLys } as state) =
    let
        f =
            bindTiltak this state

        verdisettinger =
            GeneralForutsetninger.verdisettinger

        overfoertNytte =
            Maybe.map3 (\a b c -> a * b * c)
                (Just fotgjengerLEDTotalReiseDistanceKm)
                (nyeGangturerFra this state verdisettinger.andelNyeFotgjengereFraBil)
                (Just verdisettinger.koekostnadBiler)
    in
    Maybe.map2 (+) (f .yearlyTrafikantNytte) overfoertNytte


yearlyTrafikantNytteInklOverfoert this ({ ledLys } as state) =
    Maybe.map2 (+)
        (syklistYearlyTrafikantNytteInklOverfoert this state)
        (fotgjengerYearlyTrafikantNytteInklOverfoert this state)


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
            if lengde > syklistLEDTotalReiseDistanceKm then
                nytteKalkulasjon turerPerYear syklistLEDTotalReiseDistanceKm

            else
                nytteKalkulasjon turerPerYear lengde
        )
        ledLys.sykkelturerPerYear.value
        ledLys.lengdeSykkelveiKm.value


syklistYearlyHelsegevinstNytteInklOverfoert this state =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
    Maybe.map3
        (\a b c -> a * b * c)
        (Maybe.map2 (\a b -> a - b)
            (yearlyOverfoerteSykkelturer this state)
            (nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraGange)
        )
        (Just syklistLEDTotalReiseDistanceKm)
        (Just verdisettinger.helseTSGevinstSykkel)


fotgjengerYearlyHelsegevinstNytteInklOverfoert this state =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
    Maybe.map3
        (\a b c -> a * b * c)
        (Maybe.map2 (\a b -> a - b)
            (yearlyOverfoerteGangturer this state)
            (nyeGangturerFra this state verdisettinger.andelNyeFotgjengereFraSykkel)
        )
        (Just fotgjengerLEDTotalReiseDistanceKm)
        (Just verdisettinger.helseTSGevinstGange)


yearlyHelsegevinstNytteInklOverfoert this state =
    Maybe.map2 (+)
        (syklistYearlyHelsegevinstNytteInklOverfoert this state)
        (fotgjengerYearlyHelsegevinstNytteInklOverfoert this state)


yearlyTSGevinstNytteOverfoert this ({ ledLys } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        nyeSykkelturerFraBilMaybe =
            nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraBil

        nyeSykkelturerFraKollektivMaybe =
            nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraKollektivtransport

        nyeSykkelturerFraGangeMaybe =
            nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraGange

        nyeSykkelturerFraGenererteMaybe =
            nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterGenererte

        helper nyeSykkelturerFraBil nyeSykkelturerFraKollektiv nyeSykkelturerFraGange nyeSykkelturerFraGenererte =
            nyeSykkelturerFraBil
                * (verdisettinger.tsKostnadBil
                    - verdisettinger.tsKostnadSykkel
                    * (1 - verdisettinger.tsGevinstLEDLysSyklende)
                  )
                + nyeSykkelturerFraKollektiv
                * (verdisettinger.tsKostnadKollektiv
                    - verdisettinger.tsKostnadSykkel
                    * (1 - verdisettinger.tsGevinstLEDLysSyklende)
                  )
                + nyeSykkelturerFraGange
                * (verdisettinger.tsKostnadGange
                    - verdisettinger.tsKostnadSykkel
                    * (1 - verdisettinger.tsGevinstLEDLysSyklende)
                  )
                - nyeSykkelturerFraGenererte
                * verdisettinger.tsKostnadSykkel
                * (1 - verdisettinger.tsGevinstLEDLysSyklende)
    in
    Maybe.map2 (*)
        (Just syklistLEDTotalReiseDistanceKm)
        (Maybe.map4
            helper
            nyeSykkelturerFraBilMaybe
            nyeSykkelturerFraKollektivMaybe
            nyeSykkelturerFraGangeMaybe
            nyeSykkelturerFraGenererteMaybe
        )


yearlyTSGevinstNytteInklOverfoert : StateCalculationMethod
yearlyTSGevinstNytteInklOverfoert this ({ ledLys } as state) =
    Maybe.map2 (+)
        (yearlyTSGevinstNytte this state)
        (yearlyTSGevinstNytteOverfoert this state)


yearlyEksterneEffekterNytteInklOverfoert : StateCalculationMethod
yearlyEksterneEffekterNytteInklOverfoert this ({ ledLys } as state) =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger

        nytte nyeSykkelturerFraBil nyeSykkelturerFraKollektiv =
            syklistLEDTotalReiseDistanceKm * (nyeSykkelturerFraBil * (verdisettinger.eksterneKostnaderBil - verdisettinger.eksterneKostnaderSykkel) + nyeSykkelturerFraKollektiv * (verdisettinger.eksterneKostnaderKollektiv - verdisettinger.eksterneKostnaderSykkel))

        nyeSykkelturerFraBilMaybe =
            nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraBil

        nyeSykkelturerFraKollektivMaybe =
            nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraKollektivtransport
    in
    Maybe.map2 nytte nyeSykkelturerFraBilMaybe nyeSykkelturerFraKollektivMaybe


yearlyOverfoerteSykkelturer : StateCalculationMethod
yearlyOverfoerteSykkelturer this state =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
    Maybe.map4 (\a b c d -> a + b + c + d)
        (nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraBil)
        (nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraKollektivtransport)
        (nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterFraGange)
        (nyeSykkelturerFra this state verdisettinger.andelNyeSyklisterGenererte)


yearlyOverfoerteGangturer : StateCalculationMethod
yearlyOverfoerteGangturer this state =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
    Maybe.map4 (\a b c d -> a + b + c + d)
        (nyeGangturerFra this state verdisettinger.andelNyeFotgjengereFraBil)
        (nyeGangturerFra this state verdisettinger.andelNyeFotgjengereFraKollektivtransport)
        (nyeGangturerFra this state verdisettinger.andelNyeFotgjengereFraSykkel)
        (nyeGangturerFra this state verdisettinger.andelNyeFotgjengereGenererte)


nyeSykkelturerFra this ({ ledLys } as state) prosentAndel =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
    Maybe.map3
        (\a b c -> a * b * c)
        ledLys.sykkelturerPerYear.value
        (Just verdisettinger.sykkelBedreBelysningLED)
        (Just prosentAndel)


nyeGangturerFra this ({ ledLys } as state) prosentAndel =
    let
        verdisettinger =
            GeneralForutsetninger.verdisettinger
    in
    Maybe.map3
        (\a b c -> a * b * c)
        ledLys.gangturerPerYear.value
        (Just verdisettinger.fotgjengerBedreBelysningLED)
        (Just prosentAndel)


ledTidsbesparelseMinutterPerTur : Float
ledTidsbesparelseMinutterPerTur =
    0.5


syklistLEDTotalReiseDistanceKm =
    5


fotgjengerLEDTotalReiseDistanceKm =
    2


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
                        0
        }


initialState : LEDLysState
initialState =
    { installationCost = formattedValueDefault
    , yearlyMaintenance = formattedValueDefault
    , sykkelturerPerYear = formattedValueDefault
    , gangturerPerYear = formattedValueDefault
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

module Tiltak.Vinterdrift exposing (..)

import BasicState exposing (Nivaa(..), Sted(..))
import BasicTiltak
import Field exposing (SimpleField)
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
        )
import Models exposing (Field, FieldSpec(..))
import SpecificStates exposing (VinterdriftState)
import Tiltak exposing (..)
import TiltakForutsetninger
import TiltakSupport


tiltak : Tiltak
tiltak =
    let
        basicTiltakRecord =
            BasicTiltak.basicTiltakRecord tiltakRecordImplementation
    in
    Tiltak
        { basicTiltakRecord
            | yearlyFotgjengerNytteInklOverfoert =
                \this state ->
                    Maybe.map2 (*)
                        state.vinterdrift.oppetidPercent.value
                        (basicTiltakRecord.yearlyFotgjengerNytteInklOverfoert this state)
            , yearlySyklistNytteInklOverfoert =
                \this state ->
                    Maybe.map2 (*)
                        state.vinterdrift.oppetidPercent.value
                        (basicTiltakRecord.yearlySyklistNytteInklOverfoert this state)
        }


tiltakRecordImplementation : Hooks VinterdriftState
tiltakRecordImplementation =
    { title = \_ -> "Vinterdrift"
    , nivaaTitle =
        \nivaa ->
            case nivaa of
                LavTilMiddels ->
                    "Fra ingen vinterdrift til standard brøyting-strøing (GsB)"

                LavTilHoey ->
                    "Fra ingen vinterdrift til kosting-salting (GsA)"

                MiddelsTilHoey ->
                    "Fra standard brøyting-strøing (GsB) til kosting-salting (GsA)"
    , fields = \_ -> fields
    , specificStateFocus = specificState
    , investeringsKostInklRestverdi =
        \_ { vinterdrift } ->
            TiltakSupport.investeringsKostInklRestverdi
                vinterdrift
                levetid
    , basicState =
        \{ vinterdrift } ->
            BasicState.createBasicState vinterdrift
    , nivaaFocus = specificState => FormattedValue.nivaa
    , stedFocus = specificState => FormattedValue.sted
    , syklistForutsetninger = syklistForutsetninger
    , fotgjengerForutsetninger = fotgjengerForutsetninger
    , nivaaForutsetninger = nivaaForutsetninger
    }


initialState : VinterdriftState
initialState =
    { nivaa = LavTilHoey
    , sted = Storby
    , installationCost = Just 0 |> formattedValue
    , sykkelturerPerYear = Just 0 |> formattedValue
    , gangturerPerYear = Just 0 |> formattedValue
    , lengdeVeiKm = formattedValueDefault
    , oppetidPercent = Just 0.8 |> formattedValue
    , preferredToGraph = ""
    }


specificState :
    Focus
        { tiltakStates
            | vinterdrift : VinterdriftState
        }
        VinterdriftState
specificState =
    Focus.create
        .vinterdrift
        (\f tiltakStates ->
            { tiltakStates
                | vinterdrift = f tiltakStates.vinterdrift
            }
        )


fields : List Field
fields =
    fieldDefinitions
        |> Field.transformToFields


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
    [ Field.lengdeVeiKmSimpleField specificState
    , Field.sykkelturerPerYearSimpleField specificState
    , Field.gangturerPerYearSimpleField specificState
    , { name = "oppetidPercent"
      , title = "Tiltakets oppetid, prosent"
      , placeholder = "Andel av aktuell tidsperiode hvor nivået GsA oppfylles (mindre enn 100% pga f.eks. at det tar tid fra nedbør skjer, til GsA-standard er gjenopprettet)"
      , focus = specificState => oppetidPercent
      , fieldSpec = PercentSpec
      }
    ]


levetid =
    40


nivaaForutsetninger :
    Tiltak
    -> TiltakStates
    -> NivaaForutsetninger
nivaaForutsetninger ((Tiltak object) as this) state =
    let
        basicState =
            object.basicState state

        hastighet =
            { syklende =
                { lav = 13.1, middels = 15.7, hoey = 17 }
            , gaaende =
                { lav = 4.4, middels = 4.9, hoey = 5.3 }
            }

        tidsbesparelseMinutterPerKilometer fraKmt tilKmt =
            (1 / fraKmt - 1 / tilKmt) * 60
    in
    case basicState.nivaa of
        LavTilHoey ->
            { annuiserteDriftsKostnaderPerKm = 195000
            , etterspoerselsEffekt = 5 / 100
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.syklende.lav
                    hastighet.syklende.hoey
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.gaaende.lav
                    hastighet.gaaende.hoey
            , tsGevinstSyklende = 0.027531957
            , tsGevinstGaaende = 0.334442596
            , wtp = 3.16
            }

        LavTilMiddels ->
            { annuiserteDriftsKostnaderPerKm = 37000
            , etterspoerselsEffekt = 4 / 100
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.syklende.lav
                    hastighet.syklende.middels
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.gaaende.lav
                    hastighet.gaaende.middels
            , tsGevinstSyklende = 0.013765978
            , tsGevinstGaaende = 0.141430948
            , wtp = 2.51
            }

        MiddelsTilHoey ->
            { annuiserteDriftsKostnaderPerKm = 158000
            , etterspoerselsEffekt = 1 / 100
            , tidsbesparelseSyklendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.syklende.middels
                    hastighet.syklende.hoey
            , tidsbesparelseGaaendeMinutterPerKilometer =
                tidsbesparelseMinutterPerKilometer
                    hastighet.gaaende.middels
                    hastighet.gaaende.hoey
            , tsGevinstSyklende = 0.013958126
            , tsGevinstGaaende = 0.224806202
            , wtp = 0.65
            }


syklistForutsetninger : Tiltak -> TiltakStates -> BrukerForutsetninger
syklistForutsetninger this state =
    let
        basic =
            TiltakForutsetninger.basicSyklistForutsetninger this state

        receiver =
            bindTiltak this state
    in
    { basic
        | tsGevinstTiltak = (receiver .nivaaForutsetninger).tsGevinstSyklende
    }


fotgjengerForutsetninger : Tiltak -> TiltakStates -> BrukerForutsetninger
fotgjengerForutsetninger ((Tiltak object) as this) state =
    let
        basic =
            TiltakForutsetninger.basicFotgjengerForutsetninger this state

        receiver =
            bindTiltak this state
    in
    { basic
        | tsGevinstTiltak = (receiver .nivaaForutsetninger).tsGevinstGaaende
    }


yearlyGangturer : Tiltak -> Tiltak.TiltakStates -> Maybe Float
yearlyGangturer this state =
    fotgjengerForutsetninger this state |> TiltakSupport.yearlyOverfoerteTurer this state

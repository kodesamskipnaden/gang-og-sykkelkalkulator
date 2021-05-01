module TiltakSupport exposing (..)

import Field exposing (Field, FieldSpec(..))
import Focus exposing (Focus)
import FormattedValue
    exposing
        ( Editable(..)
        , FormattedValue
        , installationCost
        , state
        , value
        )
import GeneralForutsetninger exposing (verdisettinger)
import Maybe.Extra
import Regex
import Tiltak exposing (..)


type alias SimpleField =
    { name : String
    , title : String
    , placeholder : String
    , fieldSpec : FieldSpec

    -- focus for TiltakStates to fields value
    , focus : Focus TiltakStates (FormattedValue Float)
    }


transformToFields : List SimpleField -> List Field
transformToFields fieldDefinitions =
    let
        toRealField simpleField =
            { name = simpleField.name
            , title = simpleField.title
            , placeholder = simpleField.placeholder
            , fieldSpec = simpleField.fieldSpec
            , focus = simpleField.focus
            , isEditable =
                \tiltakStates ->
                    case Focus.get (Focus.join simpleField.focus state) tiltakStates of
                        Edit ->
                            True

                        Display ->
                            False
            , beDisplayMode =
                \tiltakStates ->
                    Focus.set (Focus.join simpleField.focus state) Display tiltakStates
            , beEditMode =
                \tiltakStates ->
                    Focus.set (Focus.join simpleField.focus state) Edit tiltakStates
            , value = Focus.get (Focus.join simpleField.focus value)
            }
    in
    fieldDefinitions
        |> List.map toRealField


lengdeVeiKmSimpleField :
    Focus TiltakStates { b | lengdeVeiKm : FormattedValue Float }
    -> SimpleField
lengdeVeiKmSimpleField specificState =
    { name = "lengdeVeiKm"
    , title = "Veilengde i kilometer"
    , placeholder = "Lengde vei (km)"
    , fieldSpec = FloatSpec { stepSize = 1 }
    , focus = Focus.join specificState FormattedValue.lengdeVeiKm
    }


sykkelturerPerYearSimpleField :
    Focus TiltakStates { specificState | sykkelturerPerYear : FormattedValue Float }
    -> SimpleField
sykkelturerPerYearSimpleField specificState =
    { name = "sykkelturerPerYear"
    , title = "Antall sykkelturer per år"
    , placeholder = "Sykkelturer som får nytte av tiltaket"
    , focus = Focus.join specificState FormattedValue.sykkelturerPerYear
    , fieldSpec = IntSpec { stepSize = 50 }
    }


gangturerPerYearSimpleField :
    Focus TiltakStates { specificState | gangturerPerYear : FormattedValue Float }
    -> SimpleField
gangturerPerYearSimpleField specificState =
    { name = "gangturerPerYear"
    , title = "Antall gangturer per år"
    , placeholder = "Gangturer som får nytte av tiltaket"
    , focus = Focus.join specificState FormattedValue.gangturerPerYear
    , fieldSpec = IntSpec { stepSize = 50 }
    }



{--
these are not valid in id's for css selectors which is what we use

!"#$%&'()*+,./:;<=>?@[\]^`{|}~

the toDomId function should probably just validate with a white-list
rather than a black list like it does now

--}


userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


toDomId : String -> String
toDomId string =
    string
        -- add all invalid characters in domId here
        |> userReplace "[:/]" (\_ -> " ")
        -- whitespace is handled here
        |> String.words
        |> String.join "-"


maybeSum : List (Maybe number) -> Maybe number
maybeSum listOfMaybes =
    Maybe.Extra.combine listOfMaybes |> Maybe.map List.sum


allYearlyNytter : Tiltak -> TiltakStates -> List (Maybe Float)
allYearlyNytter this state =
    let
        f accessor =
            sendTo this accessor state
    in
    [ f .yearlySyklistNytteInklOverfoert
    , f .yearlyFotgjengerNytteInklOverfoert
    , f .yearlyTrafikantNytteInklOverfoert
    , f .yearlyHelsegevinstNytteInklOverfoert
    , f .yearlyTSGevinstNytteInklOverfoert
    , f .yearlyEksterneEffekterNytteInklOverfoert
    ]


yearlyNytteInklOverfoertSum : StateCalculationMethod
yearlyNytteInklOverfoertSum this state =
    allYearlyNytter this state |> maybeSum


nytteInklOverfoert : StateCalculationMethod
nytteInklOverfoert this state =
    allYearlyNytter this state |> List.map (Maybe.map ((*) GeneralForutsetninger.afaktorVekst)) |> maybeSum


nettoNytteInklOverfoert : StateCalculationMethod
nettoNytteInklOverfoert this state =
    let
        f =
            bindTiltak this state
    in
    Maybe.map3 (\a b c -> a + b + c)
        (f .nytteInklOverfoert)
        (f .kostUtenSkyggepris)
        (f .skyggepris)


syklistNytteInklOverfoert : StateCalculationMethod
syklistNytteInklOverfoert =
    analysePeriodeNytteFor .yearlySyklistNytteInklOverfoert


fotgjengerNytteInklOverfoert : StateCalculationMethod
fotgjengerNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyFotgjengerNytteInklOverfoert


trafikantNytteInklOverfoert : StateCalculationMethod
trafikantNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyTrafikantNytteInklOverfoert


helseGevinstNytteInklOverfoert : StateCalculationMethod
helseGevinstNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyHelsegevinstNytteInklOverfoert


tsGevinstNytteInklOverfoert : StateCalculationMethod
tsGevinstNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyTSGevinstNytteInklOverfoert


eksterneEffekterNytteInklOverfoert : StateCalculationMethod
eksterneEffekterNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyEksterneEffekterNytteInklOverfoert


analysePeriodeNytteFor :
    (TiltakRecord -> Tiltak -> TiltakStates -> Maybe Float)
    -> Tiltak
    -> TiltakStates
    -> Maybe Float
analysePeriodeNytteFor accessor this state =
    sendTo this accessor state |> Maybe.map ((*) GeneralForutsetninger.afaktorVekst)


kostUtenSkyggepris : StateCalculationMethod
kostUtenSkyggepris this state =
    let
        f =
            bindTiltak this state
    in
    Maybe.map2 (+)
        (f .investeringsKostInklRestverdi)
        (f .driftOgVedlihKost)


skyggeprisHelper : StateCalculationMethod
skyggeprisHelper this state =
    sendTo this .kostUtenSkyggepris state
        |> Maybe.map ((*) GeneralForutsetninger.skyggepris)


tidsbesparelseMinPerTurSyklende : StateCalculationMethod
tidsbesparelseMinPerTurSyklende ((Tiltak object) as this) state =
    let
        receiver =
            bindTiltak this state

        basicState =
            object.basicState state

        tidsbesparelseMinPerKm =
            (receiver .nivaaForutsetninger).tidsbesparelseSyklendeMinutterPerKilometer
    in
    Maybe.map2 (*)
        basicState.lengdeVeiKm.value
        (Just tidsbesparelseMinPerKm)


tidsbesparelseMinPerTurGaaende : StateCalculationMethod
tidsbesparelseMinPerTurGaaende ((Tiltak object) as this) state =
    let
        receiver =
            bindTiltak this state

        basicState =
            object.basicState state

        tidsbesparelseMinPerKm =
            (receiver .nivaaForutsetninger).tidsbesparelseGaaendeMinutterPerKilometer
    in
    Maybe.map2 (*)
        basicState.lengdeVeiKm.value
        (Just tidsbesparelseMinPerKm)


yearlyDriftOgVedlikeholdsKostnad : StateCalculationMethod
yearlyDriftOgVedlikeholdsKostnad ((Tiltak object) as this) state =
    let
        basicState =
            object.basicState state

        receiver =
            bindTiltak this state
    in
    basicState.lengdeVeiKm.value
        |> Maybe.map (\lengde -> lengde * (receiver .nivaaForutsetninger).annuiserteDriftsKostnaderPerKm)


driftOgVedlihKost : StateCalculationMethod
driftOgVedlihKost this state =
    Maybe.map
        (\yearlyKostnad -> yearlyKostnad * GeneralForutsetninger.afaktor)
        (yearlyDriftOgVedlikeholdsKostnad this state)
        |> Maybe.map negate


nyeTurerFra :
    Tiltak
    -> TiltakStates
    -> BrukerForutsetninger
    -> (BrukerForutsetninger -> Float)
    -> Maybe Float
nyeTurerFra this state brukerForutsetninger andelsAccessor =
    let
        receiver =
            bindTiltak this state
    in
    Maybe.map3
        (\a b c -> a * b * c)
        brukerForutsetninger.turerPerYearMaybe
        (Just (receiver .nivaaForutsetninger).etterspoerselsEffekt)
        (andelsAccessor brukerForutsetninger |> Just)


yearlyOverfoerteTurer : BrukerforutsetningStateCalculationMethod
yearlyOverfoerteTurer this state brukerForutsetninger =
    let
        receiver =
            nyeTurerFra this state brukerForutsetninger
    in
    Maybe.map3 (\a b c -> a + b + c)
        (receiver .andelNyeBrukereFraBil)
        (receiver .andelNyeBrukereFraKollektivtransport)
        (receiver .andelNyeBrukereGenererte)


investeringsKostInklRestverdi :
    { specificState
        | installationCost : FormattedValue Float
    }
    -> Float
    -> Maybe Float
investeringsKostInklRestverdi specificState levetid =
    specificState
        |> Focus.get (Focus.join installationCost value)
        |> Maybe.map ((*) <| GeneralForutsetninger.investeringsFaktor levetid)
        |> Maybe.map negate

module TiltakSupport exposing (..)

import GeneralForutsetninger exposing (verifiserteVerdisettinger)
import Maybe.Extra
import Regex
import Tiltak exposing (..)



{--
these are not valid in id's for css selectors which is what we use

!"#$%&'()*+,./:;<=>?@[\]^`{|}~

the toDomId function should probably just validate with a white-list
rather than a black list like it does now

--}


toDomId : String -> String
toDomId string =
    string
        -- add all invalid characters in domId here
        |> Regex.replace Regex.All (Regex.regex "[:/]") (\_ -> " ")
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
    let
        calculation kostUtenSkyggepris =
            kostUtenSkyggepris * GeneralForutsetninger.skyggepris
    in
    sendTo this .kostUtenSkyggepris state
        |> Maybe.map calculation
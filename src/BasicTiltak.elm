module BasicTiltak exposing (..)

import Focus exposing ((=>), Focus)
import FormattedValue
    exposing
        ( FormattedValue
        , bompengeAndel
        , installationCost
        , value
        )
import GeneralForutsetninger exposing (verdisettinger)
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


nytte : StateCalculationMethod
nytte this state =
    let
        f accessor =
            sendTo this accessor state
    in
    Maybe.map4
        (\a b c d ->
            a + b + c + d
        )
        (f .syklistNytte)
        (f .fotgjengerNytte)
        (f .trafikantNytte)
        (f .tsGevinstNytte)


nytteInklOverfoert : StateCalculationMethod
nytteInklOverfoert this state =
    let
        f accessor =
            sendTo this accessor state
    in
    Maybe.Extra.combine
        [ f .syklistNytteInklOverfoert
        , f .fotgjengerNytteInklOverfoert
        , f .trafikantNytteInklOverfoert
        , f .helseGevinstNytteInklOverfoert
        , f .tsGevinstNytteInklOverfoert
        , f .eksterneEffekterNytteInklOverfoert
        ]
        |> Maybe.map List.sum


nettoNytte : StateCalculationMethod
nettoNytte this state =
    let
        f =
            bindTiltak this state
    in
    Maybe.map3 (\a b c -> a + b + c)
        (f .nytte)
        (f .kostUtenSkyggepris)
        (f .skyggepris)


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


syklistNytte : StateCalculationMethod
syklistNytte =
    analysePeriodeNytteFor .yearlySyklistNytte


fotgjengerNytte : StateCalculationMethod
fotgjengerNytte =
    analysePeriodeNytteFor .yearlyFotgjengerNytte


syklistNytteInklOverfoert : StateCalculationMethod
syklistNytteInklOverfoert =
    analysePeriodeNytteFor .yearlySyklistNytteInklOverfoert


fotgjengerNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyFotgjengerNytteInklOverfoert


trafikantNytte : StateCalculationMethod
trafikantNytte =
    analysePeriodeNytteFor .yearlyTrafikantNytte


trafikantNytteInklOverfoert : StateCalculationMethod
trafikantNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyTrafikantNytteInklOverfoert


helseGevinstNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyHelsegevinstNytteInklOverfoert


tsGevinstNytte : StateCalculationMethod
tsGevinstNytte =
    analysePeriodeNytteFor .yearlyTSGevinstNytte


tsGevinstNytteInklOverfoert : StateCalculationMethod
tsGevinstNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyTSGevinstNytteInklOverfoert


eksterneEffekterNytteInklOverfoert =
    analysePeriodeNytteFor .yearlyEksterneEffekterNytteInklOverfoert


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


skyggeprisHelper this state =
    let
        calculation kostUtenSkyggepris =
            kostUtenSkyggepris * GeneralForutsetninger.skyggepris
    in
    sendTo this .kostUtenSkyggepris state
        |> Maybe.map calculation


yearlyTrafikantNytteInklOverfoert ((Tiltak object) as this) state =
    let
        nytte =
            object.yearlyTrafikantNytteInklOverfoertForBruker this state
    in
    Maybe.map2 (+)
        (object.syklistForutsetninger state |> nytte)
        (object.fotgjengerForutsetninger state |> nytte)


yearlyHelsegevinstNytteInklOverfoert ((Tiltak object) as this) state =
    let
        nytte =
            object.yearlyHelsegevinstNytteInklOverfoertForBruker this state
    in
    Maybe.map2 (+)
        (object.syklistForutsetninger state |> nytte)
        (object.fotgjengerForutsetninger state |> nytte)


yearlyTSGevinstNytte : StateCalculationMethod
yearlyTSGevinstNytte ((Tiltak object) as this) state =
    Maybe.map2 (+)
        (object.syklistForutsetninger state |> object.yearlyTSGevinstNytteForBrukere this state)
        (object.fotgjengerForutsetninger state |> object.yearlyTSGevinstNytteForBrukere this state)


yearlyTSGevinstNytteOverfoert ((Tiltak object) as this) state =
    let
        nytte =
            object.yearlyTSGevinstNytteOverfoertForBrukere this state
    in
    Maybe.map2 (+)
        (object.syklistForutsetninger state |> nytte)
        (object.fotgjengerForutsetninger state |> nytte)


yearlyTSGevinstNytteInklOverfoert : StateCalculationMethod
yearlyTSGevinstNytteInklOverfoert ((Tiltak object) as this) state =
    Maybe.map2 (+)
        (object.yearlyTSGevinstNytte this state)
        (object.yearlyTSGevinstNytteOverfoert this state)


yearlyEksterneEffekterNytteInklOverfoert ((Tiltak object) as this) state =
    let
        nytte =
            object.yearlyEksterneEffekterNytteInklOverfoertForBruker this state
    in
    Maybe.map2 (+)
        (object.syklistForutsetninger state |> nytte)
        (object.fotgjengerForutsetninger state |> nytte)


nyeTurerFra this brukerForutsetninger andelsAccessor =
    Maybe.map3
        (\a b c -> a * b * c)
        brukerForutsetninger.turerPerYearMaybe
        (Just brukerForutsetninger.etterspoerselsEffekt)
        (andelsAccessor brukerForutsetninger |> Just)


yearlyOverfoerteSykkelturer : StateCalculationMethod
yearlyOverfoerteSykkelturer ((Tiltak object) as this) state =
    object.syklistForutsetninger state |> yearlyOverfoerteTurer this


yearlyOverfoerteTurer this brukerForutsetninger =
    let
        receiver =
            nyeTurerFra this brukerForutsetninger
    in
    Maybe.map3 (\a b c -> a + b + c)
        (receiver .andelNyeBrukereFraBil)
        (receiver .andelNyeBrukereFraKollektivtransport)
        (receiver .andelNyeBrukereGenererte)


defaults =
    { syklistNytte = syklistNytte
    , fotgjengerNytte = fotgjengerNytte
    , trafikantNytte = trafikantNytte
    , tsGevinstNytte = tsGevinstNytte
    , syklistNytteInklOverfoert = syklistNytteInklOverfoert
    , fotgjengerNytteInklOverfoert = fotgjengerNytteInklOverfoert
    , trafikantNytteInklOverfoert = trafikantNytteInklOverfoert
    , tsGevinstNytteInklOverfoert = tsGevinstNytteInklOverfoert
    , helseGevinstNytteInklOverfoert = helseGevinstNytteInklOverfoert
    , eksterneEffekterNytteInklOverfoert = eksterneEffekterNytteInklOverfoert
    , nytte = nytte
    , nytteInklOverfoert = nytteInklOverfoert
    , kostUtenSkyggepris = kostUtenSkyggepris
    , nettoNytte = nettoNytte
    , nettoNytteInklOverfoert = nettoNytteInklOverfoert
    , skyggeprisHelper = skyggeprisHelper
    , graphId = \this -> sendTo this .domId |> (++) "c3graph"
    , domId = \this -> sendTo this .title |> toDomId
    , skyggepris = skyggeprisHelper
    , yearlyTSGevinstNytteOverfoert = yearlyTSGevinstNytteOverfoert
    }


basicTiltakRecord : Hooks a -> TiltakRecord
basicTiltakRecord hooks =
    { syklistNytte = defaults.syklistNytte
    , fotgjengerNytte = defaults.fotgjengerNytte
    , trafikantNytte = defaults.trafikantNytte
    , tsGevinstNytte = defaults.tsGevinstNytte
    , syklistNytteInklOverfoert = defaults.syklistNytteInklOverfoert
    , fotgjengerNytteInklOverfoert = defaults.fotgjengerNytteInklOverfoert
    , trafikantNytteInklOverfoert = defaults.trafikantNytteInklOverfoert
    , tsGevinstNytteInklOverfoert = defaults.tsGevinstNytteInklOverfoert
    , helseGevinstNytteInklOverfoert = defaults.helseGevinstNytteInklOverfoert
    , eksterneEffekterNytteInklOverfoert = defaults.eksterneEffekterNytteInklOverfoert
    , nytte = defaults.nytte
    , nytteInklOverfoert = defaults.nytteInklOverfoert
    , kostUtenSkyggepris = defaults.kostUtenSkyggepris
    , nettoNytte = defaults.nettoNytte
    , nettoNytteInklOverfoert = defaults.nettoNytteInklOverfoert
    , skyggeprisHelper = defaults.skyggeprisHelper
    , yearlyTSGevinstNytteOverfoert = defaults.yearlyTSGevinstNytteOverfoert
    , graphId = defaults.graphId
    , domId = defaults.domId
    , skyggepris = defaults.skyggepris
    , yearlySyklistNytte = \_ _ -> Nothing
    , yearlyFotgjengerNytte = \_ _ -> Nothing
    , yearlyTrafikantNytte = \_ _ -> Just 0
    , yearlyTSGevinstNytte = yearlyTSGevinstNytte
    , yearlySyklistNytteInklOverfoert = \_ _ -> Nothing
    , yearlyFotgjengerNytteInklOverfoert = \_ _ -> Nothing
    , yearlyTrafikantNytteInklOverfoert = yearlyTrafikantNytteInklOverfoert
    , yearlyTSGevinstNytteInklOverfoert = yearlyTSGevinstNytteInklOverfoert
    , yearlyHelsegevinstNytteInklOverfoert = yearlyHelsegevinstNytteInklOverfoert
    , yearlyEksterneEffekterNytteInklOverfoert = yearlyEksterneEffekterNytteInklOverfoert
    , title = hooks.title
    , fields = hooks.fields
    , preferredField = preferredField hooks.specificStateFocus
    , preferredToGraphFocus = hooks.specificStateFocus => preferredToGraph
    , driftOgVedlihKost = hooks.driftOgVedlihKost
    , investeringsKostInklRestverdi = hooks.investeringsKostInklRestverdi
    , yearlyTrafikantNytteInklOverfoertForBruker = hooks.yearlyTrafikantNytteInklOverfoertForBruker
    , yearlyHelsegevinstNytteInklOverfoertForBruker = hooks.yearlyHelsegevinstNytteInklOverfoertForBruker
    , yearlyTSGevinstNytteForBrukere = hooks.yearlyTSGevinstNytteForBrukere
    , yearlyTSGevinstNytteOverfoertForBrukere = hooks.yearlyTSGevinstNytteOverfoertForBrukere
    , yearlyEksterneEffekterNytteInklOverfoertForBruker = hooks.yearlyEksterneEffekterNytteInklOverfoertForBruker
    , syklistForutsetninger = hooks.syklistForutsetninger
    , fotgjengerForutsetninger = hooks.fotgjengerForutsetninger
    }


preferredToGraph : Focus { b | preferredToGraph : a } a
preferredToGraph =
    Focus.create
        .preferredToGraph
        (\f state -> { state | preferredToGraph = f state.preferredToGraph })


preferredField specificStateFocus tiltak tiltakStates =
    let
        fieldName =
            Focus.get
                (getAttr tiltak .preferredToGraphFocus)
                tiltakStates

        filterByName field =
            field.name == fieldName
    in
    sendTo tiltak .fields
        |> List.filter filterByName
        |> List.head


investeringsKostInklRestverdi :
    { specificState
        | installationCost : FormattedValue Float
    }
    -> Float
    -> Maybe Float
investeringsKostInklRestverdi specificState levetid =
    specificState
        |> Focus.get (installationCost => value)
        |> Maybe.map ((*) <| GeneralForutsetninger.investeringsFaktor levetid)
        |> Maybe.map negate


driftOgVedlihKost : { specificState | yearlyMaintenance : FormattedValue Float } -> Maybe Float
driftOgVedlihKost specificState =
    specificState.yearlyMaintenance.value
        |> Maybe.map ((*) GeneralForutsetninger.afaktor)
        |> Maybe.map negate


basicSyklistForutsetninger sykkelturerPerYearMaybe =
    { andelNyeBrukereFraBil = verdisettinger.andelNyeSyklisterFraBil
    , andelNyeBrukereFraKollektivtransport = verdisettinger.andelNyeSyklisterFraKollektivtransport
    , andelNyeBrukereGenererte = verdisettinger.andelNyeSyklisterGenererte
    , tsKostnad = verdisettinger.tsKostnadSykkel
    , eksterneKostnader = verdisettinger.eksterneKostnaderSykkel
    , turerPerYearMaybe = sykkelturerPerYearMaybe
    , totalReiseDistanceKm = verdisettinger.syklistTotalReiseDistanceKm
    , helseTSGevinstBruker = verdisettinger.helseTSGevinstSykkel
    , tsGevinstTiltak = 0
    , etterspoerselsEffekt = 0
    }


basicFotgjengerForutsetninger gangturerPerYearMaybe =
    { andelNyeBrukereFraBil = verdisettinger.andelNyeFotgjengereFraBil
    , andelNyeBrukereFraKollektivtransport = verdisettinger.andelNyeFotgjengereFraKollektivtransport
    , andelNyeBrukereGenererte = verdisettinger.andelNyeFotgjengereGenererte
    , tsKostnad = verdisettinger.tsKostnadGange
    , eksterneKostnader = verdisettinger.eksterneKostnaderGange
    , totalReiseDistanceKm = verdisettinger.fotgjengerTotalReiseDistanceKm
    , helseTSGevinstBruker = verdisettinger.helseTSGevinstGange
    , turerPerYearMaybe = gangturerPerYearMaybe
    , tsGevinstTiltak = 0
    , etterspoerselsEffekt = 0
    }

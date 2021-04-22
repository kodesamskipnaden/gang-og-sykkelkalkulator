module BasicTiltak exposing (..)

import Focus exposing ((=>), Focus)
import FormattedValue
    exposing
        ( FormattedValue
        , bompengeAndel
        , installationCost
        , value
        )
import GeneralForutsetninger exposing (verifiserteVerdisettinger)
import Tiltak exposing (..)
import TiltakForutsetninger
import TiltakSupport


yearlyTrafikantNytteInklOverfoert : StateCalculationMethod
yearlyTrafikantNytteInklOverfoert ((Tiltak object) as this) state =
    let
        receiver =
            bindTiltak this state

        nytte =
            receiver .yearlyTrafikantNytteInklOverfoertForBruker
    in
    Maybe.map2 (+)
        (receiver .syklistForutsetninger |> nytte)
        (receiver .fotgjengerForutsetninger |> nytte)


yearlyHelsegevinstNytteInklOverfoert : StateCalculationMethod
yearlyHelsegevinstNytteInklOverfoert ((Tiltak object) as this) state =
    let
        nytte =
            object.yearlyHelsegevinstNytteInklOverfoertForBruker this state
    in
    Maybe.map2 (+)
        (object.syklistForutsetninger this state |> nytte)
        (object.fotgjengerForutsetninger this state |> nytte)


yearlyTSGevinstNytte : StateCalculationMethod
yearlyTSGevinstNytte ((Tiltak object) as this) state =
    Maybe.map2 (+)
        (object.syklistForutsetninger this state |> object.yearlyTSGevinstNytteForBrukere this state)
        (object.fotgjengerForutsetninger this state |> object.yearlyTSGevinstNytteForBrukere this state)


yearlyTSGevinstNytteForBrukere ((Tiltak record) as this) state brukerForutsetninger =
    Maybe.map2
        (\turerPerYear lengde ->
            min lengde brukerForutsetninger.totalReiseDistanceKm
                * turerPerYear
                * brukerForutsetninger.tsKostnad
                * brukerForutsetninger.tsGevinstTiltak
        )
        brukerForutsetninger.turerPerYearMaybe
        (record.basicState state).lengdeVeiKm.value


yearlyTSGevinstNytteOverfoert ((Tiltak object) as this) state =
    let
        nytte =
            object.yearlyTSGevinstNytteOverfoertForBrukere this state
    in
    Maybe.map2 (+)
        (object.syklistForutsetninger this state |> nytte)
        (object.fotgjengerForutsetninger this state |> nytte)


yearlyTSGevinstNytteOverfoertForBrukere ((Tiltak object) as this) state brukerForutsetninger =
    let
        nyeTurerFunc =
            nyeTurerFra this state brukerForutsetninger

        tsKostnader =
            (object.basicState state).sted |> TiltakForutsetninger.stedsForutsetninger |> .tsKostnader

        beregning nyeTurerFraBil nyeTurerFraKollektiv nyeTurerFraGenererte =
            brukerForutsetninger.totalReiseDistanceKm
                * (nyeTurerFraBil
                    * (tsKostnader.bil
                        - brukerForutsetninger.tsKostnad
                      )
                    + (nyeTurerFraKollektiv
                        * (tsKostnader.kollektivtransport
                            - brukerForutsetninger.tsKostnad
                          )
                      )
                    - nyeTurerFraGenererte
                    * brukerForutsetninger.tsKostnad
                  )
    in
    Maybe.map3
        beregning
        (nyeTurerFunc .andelNyeBrukereFraBil)
        (nyeTurerFunc .andelNyeBrukereFraKollektivtransport)
        (nyeTurerFunc .andelNyeBrukereGenererte)


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
        (object.syklistForutsetninger this state |> nytte)
        (object.fotgjengerForutsetninger this state |> nytte)


nyeTurerFra ((Tiltak object) as this) state brukerForutsetninger andelsAccessor =
    let
        receiver =
            bindTiltak this state
    in
    Maybe.map3
        (\a b c -> a * b * c)
        brukerForutsetninger.turerPerYearMaybe
        (Just (receiver .nivaaForutsetninger).etterspoerselsEffekt)
        (andelsAccessor brukerForutsetninger |> Just)


yearlyOverfoerteSykkelturer : StateCalculationMethod
yearlyOverfoerteSykkelturer ((Tiltak object) as this) state =
    object.syklistForutsetninger this state |> yearlyOverfoerteTurer this state


yearlyOverfoerteGangturer ((Tiltak object) as this) state =
    object.fotgjengerForutsetninger this state |> yearlyOverfoerteTurer this state


yearlyOverfoerteTurer this state brukerForutsetninger =
    let
        receiver =
            nyeTurerFra this state brukerForutsetninger
    in
    Maybe.map3 (\a b c -> a + b + c)
        (receiver .andelNyeBrukereFraBil)
        (receiver .andelNyeBrukereFraKollektivtransport)
        (receiver .andelNyeBrukereGenererte)


yearlyTrafikantNytteInklOverfoertForBruker ((Tiltak object) as this) state brukerForutsetninger =
    let
        receiver =
            bindTiltak this state

        basicState =
            object.basicState state

        koekostnad =
            (TiltakForutsetninger.stedsForutsetninger basicState.sted).koekostnadBiler
    in
    Maybe.map3 (\a b c -> a * b * c)
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (nyeTurerFra this state brukerForutsetninger .andelNyeBrukereFraBil)
        (Just koekostnad)


yearlySyklistNytteInklOverfoert : StateCalculationMethod
yearlySyklistNytteInklOverfoert ((Tiltak object) as this) state =
    let
        receiver =
            bindTiltak this state

        boundSyklistForutsetninger =
            receiver .syklistForutsetninger

        overfoertNytte =
            Maybe.map
                -- bare halvparten nytte for overførte turer
                (\a -> a / 2)
                (boundSyklistForutsetninger
                    |> yearlyOverfoerteTurer this state
                    |> yearlyDirekteNyttePerTurForBruker this state boundSyklistForutsetninger
                )
    in
    Maybe.map3 (\a b c -> a + b + c)
        (receiver .yearlySyklistNytte)
        overfoertNytte
        (receiver .wtpNytte boundSyklistForutsetninger)


yearlyFotgjengerNytteInklOverfoert this state =
    let
        receiver =
            bindTiltak this state

        boundFotgjengerForutsetninger =
            receiver .fotgjengerForutsetninger

        overfoertNytte =
            Maybe.map
                (\fotgjengerNytte ->
                    fotgjengerNytte / 2
                )
                (boundFotgjengerForutsetninger
                    |> yearlyOverfoerteTurer this state
                    |> receiver .yearlyFotgjengerNyttePerTur
                )
    in
    Maybe.map3 (\a b c -> a + b + c)
        (receiver .yearlyFotgjengerNytte)
        overfoertNytte
        (receiver .wtpNytte boundFotgjengerForutsetninger)


yearlyHelsegevinstNytteInklOverfoertForBruker this state brukerForutsetninger =
    Maybe.map3
        (\a b c -> a * b * c)
        (yearlyOverfoerteTurer this state brukerForutsetninger)
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (Just brukerForutsetninger.helseGevinstBruker)


yearlySyklistNytte : StateCalculationMethod
yearlySyklistNytte ((Tiltak object) as this) state =
    let
        receiver =
            bindTiltak this state
    in
    receiver .syklistForutsetninger |> yearlyDirekteNytteForBruker this state


yearlyFotgjengerNytte ((Tiltak object) as this) state =
    object.yearlyFotgjengerNyttePerTur this state (object.basicState state).gangturerPerYear.value


yearlyEksterneEffekterNytteInklOverfoertForBruker ((Tiltak object) as this) state brukerForutsetninger =
    let
        nyeTurer =
            nyeTurerFra this state brukerForutsetninger

        sted =
            (object.basicState state).sted

        eksterneKostnader =
            TiltakForutsetninger.stedsForutsetninger sted |> .oevrigeEksterneKostnader

        overfoertFraBilNyttePerKm nyeTurerFraBil =
            nyeTurerFraBil * eksterneKostnader.bil

        overfoertFraKollektivNyttePerKm nyeTurerFraKollektiv =
            nyeTurerFraKollektiv * eksterneKostnader.kollektivtransport

        nytte nyeTurerFraBil nyeTurerFraKollektiv =
            brukerForutsetninger.totalReiseDistanceKm
                * (overfoertFraBilNyttePerKm nyeTurerFraBil
                    + overfoertFraKollektivNyttePerKm nyeTurerFraKollektiv
                  )
    in
    Maybe.map2 nytte
        (nyeTurer .andelNyeBrukereFraBil)
        (nyeTurer .andelNyeBrukereFraKollektivtransport)


yearlySyklistNyttePerTur this state antallTurer =
    let
        receiver =
            bindTiltak this state

        brukerForutsetninger =
            receiver .syklistForutsetninger
    in
    yearlyDirekteNyttePerTurForBruker this state brukerForutsetninger antallTurer


yearlyFotgjengerNyttePerTur this state antallTurer =
    let
        receiver =
            bindTiltak this state

        brukerForutsetninger =
            receiver .fotgjengerForutsetninger
    in
    yearlyDirekteNyttePerTurForBruker this state brukerForutsetninger antallTurer


yearlyDirekteNyttePerTurForBruker this state brukerForutsetninger antallTurerMaybe =
    Maybe.map2
        (\a b -> a * b * brukerForutsetninger.voTBruker)
        antallTurerMaybe
        brukerForutsetninger.tidsbesparelseMinPerTur


yearlyDirekteNytteForBruker this state brukerForutsetninger =
    yearlyDirekteNyttePerTurForBruker
        this
        state
        brukerForutsetninger
        brukerForutsetninger.turerPerYearMaybe


wtpNytte ((Tiltak object) as this) state brukerForutsetninger =
    let
        basicState =
            object.basicState state

        totalReiseDistanceKm =
            brukerForutsetninger.totalReiseDistanceKm

        turerPerYearMaybe =
            brukerForutsetninger.turerPerYearMaybe

        distanseMaybe =
            Maybe.map
                (\lengdeVei -> min lengdeVei totalReiseDistanceKm)
                basicState.lengdeVeiKm.value

        receiver =
            bindTiltak this state

        wtp =
            (receiver .nivaaForutsetninger).wtp

        turerPlussMaybe =
            Maybe.map2
                (\antallTurer overfoerteTurer -> antallTurer + 0.5 * overfoerteTurer)
                turerPerYearMaybe
                (brukerForutsetninger |> yearlyOverfoerteTurer this state)
    in
    Maybe.map2 (\distanse turerPluss -> distanse * turerPluss * wtp)
        distanseMaybe
        turerPlussMaybe


yearlyDriftOgVedlikeholdsKostnad ((Tiltak object) as this) state =
    let
        basicState =
            object.basicState state

        receiver =
            bindTiltak this state
    in
    basicState.lengdeVeiKm.value
        |> Maybe.map (\lengde -> lengde * (receiver .nivaaForutsetninger).annuiserteDriftsKostnaderPerKm)


driftOgVedlihKost ((Tiltak object) as this) state =
    Maybe.map
        (\yearlyKostnad -> yearlyKostnad * GeneralForutsetninger.afaktor)
        (yearlyDriftOgVedlikeholdsKostnad this state)
        |> Maybe.map negate


defaults =
    { syklistNytteInklOverfoert = TiltakSupport.syklistNytteInklOverfoert
    , fotgjengerNytteInklOverfoert = TiltakSupport.fotgjengerNytteInklOverfoert
    , trafikantNytteInklOverfoert = TiltakSupport.trafikantNytteInklOverfoert
    , tsGevinstNytteInklOverfoert = TiltakSupport.tsGevinstNytteInklOverfoert
    , helseGevinstNytteInklOverfoert = TiltakSupport.helseGevinstNytteInklOverfoert
    , eksterneEffekterNytteInklOverfoert = TiltakSupport.eksterneEffekterNytteInklOverfoert
    , nytteInklOverfoert = TiltakSupport.nytteInklOverfoert
    , kostUtenSkyggepris = TiltakSupport.kostUtenSkyggepris
    , nettoNytteInklOverfoert = TiltakSupport.nettoNytteInklOverfoert
    , skyggeprisHelper = TiltakSupport.skyggeprisHelper
    , graphId = \this -> sendTo this .domId |> (++) "c3graph"
    , domId = \this -> sendTo this .title |> TiltakSupport.toDomId
    , skyggepris = TiltakSupport.skyggeprisHelper
    , yearlyNytteInklOverfoertSum = TiltakSupport.yearlyNytteInklOverfoertSum
    , tidsbesparelseMinPerTurSyklende = TiltakSupport.tidsbesparelseMinPerTurSyklende
    , tidsbesparelseMinPerTurGaaende = TiltakSupport.tidsbesparelseMinPerTurGaaende
    }


basicTiltakRecord : Hooks a -> TiltakRecord
basicTiltakRecord hooks =
    { syklistNytteInklOverfoert = defaults.syklistNytteInklOverfoert
    , fotgjengerNytteInklOverfoert = defaults.fotgjengerNytteInklOverfoert
    , trafikantNytteInklOverfoert = defaults.trafikantNytteInklOverfoert
    , tsGevinstNytteInklOverfoert = defaults.tsGevinstNytteInklOverfoert
    , helseGevinstNytteInklOverfoert = defaults.helseGevinstNytteInklOverfoert
    , eksterneEffekterNytteInklOverfoert = defaults.eksterneEffekterNytteInklOverfoert
    , nytteInklOverfoert = defaults.nytteInklOverfoert
    , kostUtenSkyggepris = defaults.kostUtenSkyggepris
    , nettoNytteInklOverfoert = defaults.nettoNytteInklOverfoert
    , skyggeprisHelper = defaults.skyggeprisHelper
    , graphId = defaults.graphId
    , domId = defaults.domId
    , skyggepris = defaults.skyggepris
    , yearlyNytteInklOverfoertSum = defaults.yearlyNytteInklOverfoertSum
    , tidsbesparelseMinPerTurSyklende = defaults.tidsbesparelseMinPerTurSyklende
    , tidsbesparelseMinPerTurGaaende = defaults.tidsbesparelseMinPerTurGaaende
    , yearlySyklistNytte = yearlySyklistNytte
    , yearlySyklistNytteInklOverfoert = yearlySyklistNytteInklOverfoert
    , yearlyFotgjengerNytte = yearlyFotgjengerNytte
    , yearlyFotgjengerNytteInklOverfoert = yearlyFotgjengerNytteInklOverfoert
    , yearlyTSGevinstNytte = yearlyTSGevinstNytte
    , yearlyTrafikantNytteInklOverfoert = yearlyTrafikantNytteInklOverfoert
    , yearlyTrafikantNytteInklOverfoertForBruker = yearlyTrafikantNytteInklOverfoertForBruker
    , yearlyTSGevinstNytteInklOverfoert = yearlyTSGevinstNytteInklOverfoert
    , yearlyHelsegevinstNytteInklOverfoert = yearlyHelsegevinstNytteInklOverfoert
    , yearlyEksterneEffekterNytteInklOverfoert = yearlyEksterneEffekterNytteInklOverfoert
    , yearlySyklistNyttePerTur = yearlySyklistNyttePerTur
    , yearlyHelsegevinstNytteInklOverfoertForBruker = yearlyHelsegevinstNytteInklOverfoertForBruker
    , yearlyTSGevinstNytteForBrukere = yearlyTSGevinstNytteForBrukere
    , yearlyTSGevinstNytteOverfoert = yearlyTSGevinstNytteOverfoert
    , yearlyTSGevinstNytteOverfoertForBrukere = yearlyTSGevinstNytteOverfoertForBrukere
    , yearlyEksterneEffekterNytteInklOverfoertForBruker = yearlyEksterneEffekterNytteInklOverfoertForBruker
    , wtpNytte = wtpNytte
    , driftOgVedlihKost = driftOgVedlihKost
    , title = hooks.title
    , fields = hooks.fields
    , preferredField = preferredField hooks.specificStateFocus
    , preferredToGraphFocus = hooks.specificStateFocus => preferredToGraph
    , basicState = hooks.basicState
    , nivaaFocus = hooks.nivaaFocus
    , stedFocus = hooks.stedFocus
    , investeringsKostInklRestverdi = hooks.investeringsKostInklRestverdi
    , yearlyFotgjengerNyttePerTur = yearlyFotgjengerNyttePerTur
    , syklistForutsetninger = hooks.syklistForutsetninger
    , fotgjengerForutsetninger = hooks.fotgjengerForutsetninger
    , nivaaForutsetninger = hooks.nivaaForutsetninger
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

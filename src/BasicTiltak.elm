module BasicTiltak exposing (..)

import Focus exposing ((=>), Focus)
import Tiltak exposing (..)
import TiltakForutsetninger
import TiltakSupport


yearlyDirekteNyttePerTurForBruker : BrukerForutsetninger -> Maybe Float -> Maybe Float
yearlyDirekteNyttePerTurForBruker brukerForutsetninger antallTurerMaybe =
    Maybe.map2
        (\a b -> a * b * brukerForutsetninger.voTBruker)
        antallTurerMaybe
        brukerForutsetninger.tidsbesparelseMinPerTur


yearlyDirekteNytteForBruker : BrukerForutsetninger -> Maybe Float
yearlyDirekteNytteForBruker brukerForutsetninger =
    yearlyDirekteNyttePerTurForBruker
        brukerForutsetninger
        brukerForutsetninger.turerPerYearMaybe


yearlyDirekteNytteInklOverfoertForBruker : BrukerforutsetningStateCalculationMethod
yearlyDirekteNytteInklOverfoertForBruker this state brukerForutsetninger =
    let
        receiver =
            bindTiltak this state

        overfoertNytte =
            Maybe.map
                -- bare halvparten nytte for overførte turer
                (\a -> a / 2)
                (brukerForutsetninger
                    |> TiltakSupport.yearlyOverfoerteTurer this state
                    |> yearlyDirekteNyttePerTurForBruker brukerForutsetninger
                )
    in
    Maybe.map3 (\a b c -> a + b + c)
        (brukerForutsetninger |> yearlyDirekteNytteForBruker)
        overfoertNytte
        (receiver .wtpNytte brukerForutsetninger)


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


yearlyTSGevinstNytteForBrukere : BrukerforutsetningStateCalculationMethod
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


yearlyTSGevinstNytteOverfoert : StateCalculationMethod
yearlyTSGevinstNytteOverfoert ((Tiltak object) as this) state =
    let
        nytte =
            object.yearlyTSGevinstNytteOverfoertForBrukere this state
    in
    Maybe.map2 (+)
        (object.syklistForutsetninger this state |> nytte)
        (object.fotgjengerForutsetninger this state |> nytte)



-- gammel gsb_gsa syklende
-- =MIN($B$12;$B$6)*$B$10*TSkostnad_sykkel*VLOOKUP(CONCAT(F3;F4;F5);$'Forutsetninger tiltak-nivå-sted'.$F$4:$AD$39;12;FALSE()) +$B$12*F19*(VLOOKUP(CONCAT(F3;F4;F5);$'Forutsetninger tiltak-nivå-sted'.$F$4:$AD$39;14;FALSE())-TSkostnad_sykkel) +$B$12*F20*(VLOOKUP(CONCAT(F3;F4;F5);$'Forutsetninger tiltak-nivå-sted'.$F$4:$AD$39;15;FALSE())-TSkostnad_sykkel) +$B$12*F21*(TSkostnad_sykkel-TSkostnad_gange) -$B$12*F22*TSkostnad_sykkel
--
-- ny gsb_gsa syklende
-- =MIN($B$12;$B$6)*$B$10*TSkostnad_sykkel*VLOOKUP(CONCAT(I3;I4;I5);$'Forutsetninger tiltak-nivå-sted'.$F$4:$AD$39;12;FALSE()) +$B$12*I19*(VLOOKUP(CONCAT(I3;I4;I5);$'Forutsetninger tiltak-nivå-sted'.$F$4:$AD$39;14;FALSE())-TSkostnad_sykkel) +$B$12*I20*(VLOOKUP(CONCAT(I3;I4;I5);$'Forutsetninger tiltak-nivå-sted'.$F$4:$AD$39;15;FALSE())-TSkostnad_sykkel) +$B$12*I21*(TSkostnad_sykkel-TSkostnad_gange) -$B$12*I22*TSkostnad_sykkel
--
-- ny led lys syklende
-- =MIN($B$12;$B$6)*$B$10*TSkostnad_sykkel*VLOOKUP(CONCAT(F3;F4;F5);$'Forutsetninger tiltak-nivå-sted'.$F$4:$AD$39;12;FALSE()) +$B$12*F19*(VLOOKUP(CONCAT(F3;F4;F5);$'Forutsetninger tiltak-nivå-sted'.$F$4:$AD$39;14;FALSE())-TSkostnad_sykkel) +$B$12*F20*(VLOOKUP(CONCAT(F3;F4;F5);$'Forutsetninger tiltak-nivå-sted'.$F$4:$AD$39;15;FALSE())-TSkostnad_sykkel) +$B$12*F21*(TSkostnad_sykkel-TSkostnad_gange) -$B$12*F22*TSkostnad_sykkel


yearlyTSGevinstNytteOverfoertForBrukere : BrukerforutsetningStateCalculationMethod
yearlyTSGevinstNytteOverfoertForBrukere ((Tiltak object) as this) state brukerForutsetninger =
    let
        nyeTurerFunc =
            TiltakSupport.nyeTurerFra this state brukerForutsetninger

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


yearlyEksterneEffekterNytteInklOverfoert : StateCalculationMethod
yearlyEksterneEffekterNytteInklOverfoert ((Tiltak object) as this) state =
    let
        nytte =
            object.yearlyEksterneEffekterNytteInklOverfoertForBruker this state
    in
    Maybe.map2 (+)
        (object.syklistForutsetninger this state |> nytte)
        (object.fotgjengerForutsetninger this state |> nytte)


yearlyTrafikantNytteInklOverfoertForBruker : BrukerforutsetningStateCalculationMethod
yearlyTrafikantNytteInklOverfoertForBruker ((Tiltak object) as this) state brukerForutsetninger =
    let
        receiver =
            bindTiltak this state

        basicState =
            object.basicState state

        koekostnader =
            (TiltakForutsetninger.stedsForutsetninger basicState.sted).koekostnad
    in
    Maybe.map2
        (\nyeBilturer nyeKollektivTurer ->
            brukerForutsetninger.totalReiseDistanceKm
                * (nyeBilturer
                    * koekostnader.bil
                    + nyeKollektivTurer
                    * koekostnader.kollektivtransport
                  )
        )
        (TiltakSupport.nyeTurerFra this state brukerForutsetninger .andelNyeBrukereFraBil)
        (TiltakSupport.nyeTurerFra this state brukerForutsetninger .andelNyeBrukereFraKollektivtransport)


yearlyHelsegevinstNytteInklOverfoertForBruker : BrukerforutsetningStateCalculationMethod
yearlyHelsegevinstNytteInklOverfoertForBruker this state brukerForutsetninger =
    Maybe.map3
        (\a b c -> a * b * c)
        (TiltakSupport.yearlyOverfoerteTurer this state brukerForutsetninger)
        (Just brukerForutsetninger.totalReiseDistanceKm)
        (Just brukerForutsetninger.helseGevinstBruker)


yearlyEksterneEffekterNytteInklOverfoertForBruker : BrukerforutsetningStateCalculationMethod
yearlyEksterneEffekterNytteInklOverfoertForBruker ((Tiltak object) as this) state brukerForutsetninger =
    let
        nyeTurer =
            TiltakSupport.nyeTurerFra this state brukerForutsetninger

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


wtpNytte : BrukerforutsetningStateCalculationMethod
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
                (brukerForutsetninger |> TiltakSupport.yearlyOverfoerteTurer this state)
    in
    Maybe.map2 (\distanse turerPluss -> distanse * turerPluss * wtp)
        distanseMaybe
        turerPlussMaybe


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
    , driftOgVedlihKost = TiltakSupport.driftOgVedlihKost
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
    , driftOgVedlihKost = defaults.driftOgVedlihKost
    , yearlySyklistNytte =
        \this state ->
            let
                receiver =
                    bindTiltak this state
            in
            receiver .syklistForutsetninger |> yearlyDirekteNytteForBruker
    , yearlyFotgjengerNytte =
        \this state ->
            let
                receiver =
                    bindTiltak this state
            in
            receiver .fotgjengerForutsetninger |> yearlyDirekteNytteForBruker
    , yearlySyklistNytteInklOverfoert =
        \this state ->
            let
                receiver =
                    bindTiltak this state
            in
            receiver .syklistForutsetninger |> yearlyDirekteNytteInklOverfoertForBruker this state
    , yearlyFotgjengerNytteInklOverfoert =
        \this state ->
            let
                receiver =
                    bindTiltak this state
            in
            receiver .fotgjengerForutsetninger |> yearlyDirekteNytteInklOverfoertForBruker this state
    , yearlyTSGevinstNytte = yearlyTSGevinstNytte
    , yearlyTrafikantNytteInklOverfoert = yearlyTrafikantNytteInklOverfoert
    , yearlyTrafikantNytteInklOverfoertForBruker = yearlyTrafikantNytteInklOverfoertForBruker
    , yearlyTSGevinstNytteInklOverfoert = yearlyTSGevinstNytteInklOverfoert
    , yearlyHelsegevinstNytteInklOverfoert = yearlyHelsegevinstNytteInklOverfoert
    , yearlyEksterneEffekterNytteInklOverfoert = yearlyEksterneEffekterNytteInklOverfoert
    , yearlyHelsegevinstNytteInklOverfoertForBruker = yearlyHelsegevinstNytteInklOverfoertForBruker
    , yearlyTSGevinstNytteForBrukere = yearlyTSGevinstNytteForBrukere
    , yearlyTSGevinstNytteOverfoert = yearlyTSGevinstNytteOverfoert
    , yearlyTSGevinstNytteOverfoertForBrukere = yearlyTSGevinstNytteOverfoertForBrukere
    , yearlyEksterneEffekterNytteInklOverfoertForBruker = yearlyEksterneEffekterNytteInklOverfoertForBruker
    , wtpNytte = wtpNytte
    , title = hooks.title
    , fields = hooks.fields
    , preferredField = preferredField hooks.specificStateFocus
    , preferredToGraphFocus = hooks.specificStateFocus => preferredToGraph
    , basicState = hooks.basicState
    , nivaaFocus = hooks.nivaaFocus
    , stedFocus = hooks.stedFocus
    , investeringsKostInklRestverdi = hooks.investeringsKostInklRestverdi
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

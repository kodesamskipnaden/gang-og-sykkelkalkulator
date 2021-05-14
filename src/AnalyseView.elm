module AnalyseView exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg(..))
import NumberFormat
import Tiltak exposing (AnalyseData)


conclusionRow : AnalyseData -> Html Msg
conclusionRow data =
    let
        conclusionContent isProfitable =
            [ span [ class "isProfitableMark" ]
                [ text
                    (String.fromChar
                        (case isProfitable of
                            True ->
                                '✓'

                            False ->
                                '⚠'
                        )
                    )
                ]
            , text
                ("Tiltaket er "
                    ++ (case isProfitable of
                            True ->
                                ""

                            False ->
                                "ikke"
                       )
                    ++ " samfunnsøkonomisk lønnsomt."
                )
            ]
    in
    div [ class "conclusion" ]
        [ h3 [] [ text "Konklusjon" ]
        , p []
            (data.isProfitable
                |> Maybe.map conclusionContent
                |> Maybe.withDefault [ text "Ufullstendige data" ]
            )
        ]


view : AnalyseData -> List (Html Msg)
view data =
    let
        titleAndValueList =
            [ ( "Syklistenes nytte"
              , data.syklistNytte |> NumberFormat.maybePretty
              , text "Verdien i dag av syklistenes tids- og bekvemmelighetsgevinster, over hele analyseperioden"
              )
            , ( "Fotgjengernes nytte"
              , data.fotgjengerNytte |> NumberFormat.maybePretty
              , text "Verdien i dag av fotgjengernes tids- og bekvemmelighetsgevinster, over hele analyseperioden"
              )
            , ( "Øvrige trafikanters nytte"
              , data.trafikantNytte |> NumberFormat.maybePretty
              , text "Verdien i dag av effektene på øvrige trafikanter, over hele analyseperioden"
              )
            , ( "Helsegevinst"
              , data.helseGevinstNytte |> NumberFormat.maybePretty
              , text "Verdien i dag av effektene på øvrige trafikanter, over hele analyseperioden"
              )
            , ( "TS gevinst"
              , data.tsGevinstNytte |> NumberFormat.maybePretty
              , text "Verdien i dag av TS-gevinster, over hele analyseperioden"
              )
            , ( "Øvrige eksterne effekter"
              , data.eksterneEffekterNytte |> NumberFormat.maybePretty
              , text "Verdien i dag av eksterne effekter, over hele analyseperioden"
              )
            , ( "Sum nytteelementer"
              , data.nytte |> NumberFormat.maybePretty
              , text "-"
              )
            , ( "Sum kostnader (se vedlagte dokumentasjon for hvordan dette er beregnet)"
              , data.kostUtenSkyggepris |> NumberFormat.maybePretty
              , text "Verdien i dag av alle kostnader (drift/vedlikehold, investering og ev. gjeninvestering) som vil påløpe i analyseperioden"
              )
            , ( "Skyggepris offentlige midler"
              , data.skyggepris |> NumberFormat.maybePretty
              , text "Samfunnskostnaden (effektivitetstapet) som påløper når tiltaket er skattefinansiert"
              )
            , ( "Tiltakets nettonåverdi"
              , data.nettoNytte |> NumberFormat.maybePretty
              , text "Nytteelementer minus kostnadselementer. Tiltaket er lønnsomt hvis tallet er positivt."
              )
            , ( "Nettonytte per budsjettkrone (nyttekostnadsbrøk)"
              , data.nettoNyttePerBudsjettKrone |> NumberFormat.maybePrettyTwoDecimals
              , text "Uttrykk for hvor mye samfunnet får igjen for hver krone tiltaket koster. Tiltaket er lønnsomt hvis tallet er positivt"
              )
            ]

        gridRow ( title, value, popoverContent ) =
            Grid.row [ Row.attrs [ class "analyseColumn" ] ]
                [ Grid.col []
                    [ div [ class "nkaTooltip" ]
                        [ text title
                        , span [ class "nkaTooltipContent" ] [ popoverContent ]
                        ]
                    ]
                , Grid.col [ Col.attrs [ class "text-right" ] ]
                    [ text value ]
                ]

        dataRows =
            titleAndValueList |> List.map gridRow

        gridRows =
            dataRows ++ [ conclusionRow data ]
    in
    h3 [] [ text "Samfunnsøkonomisk analyse over 40 års analyseperiode" ] :: gridRows

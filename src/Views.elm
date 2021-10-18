module Views exposing (view)

import Assets
import Bootstrap.Accordion as Accordion
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Group
import Html exposing (..)
import Html.Attributes exposing (..)
import Models exposing (Group(..), Model, Page(..))
import Msgs exposing (Msg(..))
import TiltakAndGroupData
import TiltakView


groupIcon : Group -> Assets.Image
groupIcon group =
    case group of
        GruppoA ->
            Assets.informasjon


view : Model -> Html Msg
view model =
    div [ class "contents" ]
        [ mainContent model
        , appFooter
        ]


mainContent : Model -> Html Msg
mainContent model =
    case model.page of
        Home ->
            pageHome model

        NotFound ->
            pageNotFound

        GroupPage tiltaksGruppeType ->
            pageGroup tiltaksGruppeType model


groupPanel : Group -> Html Msg
groupPanel group =
    a
        [ href (Group.groupPath group)
        , class "groupPanel"
        ]
        [ Card.config []
            |> Card.block []
                [ Card.text []
                    [ img
                        [ class "groupIcon"
                        , Assets.src (groupIcon group)
                        , alt ""
                        ]
                        []
                    , div
                        [ class "group-box-title" ]
                        [ div
                            [ class "group-box-title-text" ]
                            [ text (Group.groupTitle group) ]
                        ]
                    , img
                        [ Assets.src Assets.caretRight
                        , class "caretRight"
                        , alt ""
                        ]
                        []
                    ]
                ]
            |> Card.view
        ]


haandbokV712Link =
    a
        [ href "https://www.vegvesen.no/fag/veg-og-gate/planlegging/grunnlagsdata/konsekvensanalyser"
        ]
        [ text "Håndbok v712" ]


dokumentasjonLink =
    a
        [ Assets.href Assets.dokumentasjon ]
        [ text "dokumentasjon" ]


pageHome : Model -> Html Msg
pageHome model =
    div []
        [ div [ class "jumbotron homeHeader" ]
            [ Grid.container [ class "container__narrow" ]
                [ h1 [] [ text "Nytte-kostnadskalkulator" ]
                , h2 [] [ text "for drift og vedlikehold av gang- og sykkelanlegg" ]
                , p [] [ text "Publisert mai 2021" ]
                ]
            ]
        , Grid.container [ class "groupPanels container__narrow" ]
            [ div [ class "forsidetekst" ]
                [ Grid.row []
                    [ Grid.col []
                        [ p []
                            [ text """
Velkommen til TØIs nyttekostnadsberegningsverktøy for sykkel- og gangveitiltak knyttet til vinterdrift, belysning, vegdekkestandard og renhold. Kalkulatoren følger gjeldende tilnærming og metodikk for nyttekostnadsanalyser i transportsektoren (Statens vegvesens """
                            , haandbokV712Link
                            , text """) og benytter derfor 40 års analyseperiode. 
"""
                            ]
                        , p []
                            [ text """
Klikk boksen nedenfor og velg hovedkategori av tiltak. Ved å legge inn bakgrunnsinformasjon om prosjektet, beregner kalkulatoren nytte for ulike aktører, tiltakets nettonåverdi og nettonytte per budsjettkrone (nyttekostnadsbrøk).
"""
                            ]
                        , p []
                            [ text "Beregningsopplegget er dokumentert i vedlagte "
                            , dokumentasjonLink
                            , text
                                ". Vi har også laget en kortfattet  "
                            , a [ Assets.href Assets.brukerveiledning ] [ text "veiledning" ]
                            , text " til kalkulatoren, og en samlet liste over "
                            , a [ Assets.href Assets.forutsetninger ] [ text "forutsetninger" ]
                            , text " som er brukt."
                            ]
                        ]
                    ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ groupPanel GruppoA
                    ]
                ]
            ]
        ]


pageGroup : Group -> Model -> Html Msg
pageGroup group model =
    let
        allCards =
            TiltakAndGroupData.tiltakForGroup group
                |> List.map (TiltakView.tiltakCard model.tiltakStates)

        pageHeader =
            header [ class "groupHeader" ]
                [ a [ href "#" ]
                    [ img
                        [ Assets.src Assets.backArrow
                        , class "backArrow"
                        , alt "Tilbake til forsiden"
                        ]
                        []
                    ]
                , div [ class "groupPageHeader" ]
                    [ img
                        [ class "groupIcon"
                        , Assets.src (groupIcon group)
                        , alt ""
                        ]
                        []
                    ]
                , h1 [] [ text (Group.groupTitle group) ]
                ]

        tiltakAccordions =
            Accordion.config AccordionMsg
                |> Accordion.withAnimation
                |> Accordion.cards allCards
                |> Accordion.view model.accordionState
    in
    div []
        [ Grid.containerFluid [] [ pageHeader ]
        , Grid.container [ class "container__narrow" ] [ tiltakAccordions ]
        ]


pageNotFound : Html Msg
pageNotFound =
    Grid.container [ class "container__narrow" ]
        [ h1 [] [ text "Ugyldig side" ]
        , text "Beklager, kan ikke finne siden"
        ]


appFooter : Html Msg
appFooter =
    footer [ class "footer footer-text" ]
        [ Grid.container [ class "container__narrow" ]
            [ text "Kontakt: "
            , a [ href "mailto:naf@toi.no" ] [ text "Nils Fearnley" ]
            , br [] []
            , a [ href "https://www.toi.no" ]
                [ img
                    [ Assets.src Assets.toiLogo
                    , class "toiLogo"
                    , alt "TØI logo"
                    ]
                    []
                ]
            , div [ class "colophon" ]
                [ text "Utvikling "
                , a [ href "http://github.com/kodesamskipnaden" ]
                    [ text "Kodesamskipnaden" ]
                , text " ved Syver Enstad & Knut Aksel Røysland"
                ]
            ]
        ]

module TiltakView exposing (..)

import AnalyseView
import BasicState exposing (Nivaa(..), Sted(..))
import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Field exposing (Field)
import Focus
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onFocus)
import Msgs exposing (Msg(..), RadioValue(..))
import NumberFormat
import Tiltak exposing (Tiltak(..), sendTo)
import TiltakCharting exposing (GraphState(..))
import TiltakStates exposing (TiltakStates)


fieldLabelContent : Field -> List (Html msg)
fieldLabelContent field =
    [ Html.strong [] [ text field.title ] ]


chart : TiltakStates -> Tiltak -> Html Msg
chart tiltakStates tiltak =
    let
        graphId =
            sendTo tiltak .graphId

        fieldToGraphName =
            TiltakCharting.maybeFieldToGraph
                tiltak
                tiltakStates
                |> Maybe.map .title
                |> Maybe.withDefault "WAT!!!!"

        fieldButton field =
            ButtonGroup.button
                [ Button.onClick <|
                    UpdateFieldToGraph tiltak field
                , Button.secondary
                ]
                [ text field.title ]

        alternateFieldsToGraph =
            case TiltakCharting.possibleFieldsToGraph tiltak tiltakStates of
                [] ->
                    []

                _ as fields ->
                    [ text
                        "Vis heller: "
                    , fields
                        |> List.map fieldButton
                        |> ButtonGroup.buttonGroup []
                    ]

        variableToGraphView =
            case TiltakCharting.graphState tiltak tiltakStates of
                GraphOn ->
                    [ div [ class "chartLedetekst" ]
                        [ text <| """Grafen viser hvordan tiltakets
nettonåverdi varierer med """ ++ fieldToGraphName
                        ]
                    , div []
                        alternateFieldsToGraph
                    ]

                GraphOff ->
                    []

        graphNodeContent =
            case TiltakCharting.graphState tiltak tiltakStates of
                GraphOn ->
                    []

                GraphOff ->
                    [ h3 [] [ text "Legg inn flere tall for å vise graf" ] ]
    in
    div []
        [ div [ id graphId ] graphNodeContent
        , div [] variableToGraphView
        ]


tiltakCard : TiltakStates -> Tiltak -> Accordion.Card Msg
tiltakCard tiltakStates tiltak =
    let
        analyse =
            AnalyseView.view <| Tiltak.analyse tiltak tiltakStates

        title =
            sendTo tiltak .title
    in
    Accordion.card
        { id = sendTo tiltak .domId
        , options = []
        , header = Accordion.headerH4 [] <| Accordion.toggle [] [ text title ]
        , blocks =
            [ Accordion.block
                []
                [ Card.custom <|
                    div []
                        ([ tiltakForm tiltak tiltakStates ]
                            ++ analyse
                        )
                ]
            , Accordion.block
                [ Card.blockAttrs
                    [ class "chartBlock"
                    ]
                ]
                [ Card.custom <| chart tiltakStates tiltak
                ]
            ]
        }


fieldView :
    Tiltak
    -> TiltakStates
    -> Field
    -> Html Msg
fieldView tiltak tiltakStates ({ name, title, placeholder } as field) =
    let
        isEditable =
            field.isEditable tiltakStates

        fieldValueString =
            field.value tiltakStates
                |> (case isEditable of
                        True ->
                            Maybe.map toString

                        False ->
                            Maybe.map
                                (if field.stepSize < 1 then
                                    NumberFormat.prettyTwoDecimals

                                 else
                                    NumberFormat.pretty
                                )
                   )
                |> Maybe.withDefault ""

        inputElement =
            case isEditable of
                False ->
                    Input.text

                True ->
                    Input.number
    in
    Form.group []
        [ Form.label [ for name ] (fieldLabelContent field)
        , inputElement
            [ Input.id name
            , Input.placeholder placeholder
            , Input.attrs
                [ onBlur (FieldBlur field)
                , onFocus (FieldFocus field)
                ]
            , Input.onInput <| UpdateField tiltak field
            , Input.value fieldValueString
            ]
        ]


nivaaGroup : Tiltak -> TiltakStates -> Html Msg
nivaaGroup ((Tiltak object) as tiltak) tiltakStates =
    let
        nivaa =
            Focus.get object.nivaaFocus tiltakStates
    in
    Form.group []
        ([ Form.label [ for "nivaaRadios" ] [ Html.strong [] [ text "Nivå" ] ] ]
            ++ Radio.radioList "nivaaRadios"
                [ object.nivaaTitle LavTilHoey
                    |> Radio.create
                        [ Radio.checked (nivaa == LavTilHoey)
                        , Radio.onClick (UpdateRadio tiltak (NivaaType LavTilHoey))
                        ]
                , object.nivaaTitle MiddelsTilHoey
                    |> Radio.create
                        [ Radio.checked (nivaa == MiddelsTilHoey)
                        , Radio.onClick (UpdateRadio tiltak (NivaaType MiddelsTilHoey))
                        ]
                , object.nivaaTitle LavTilMiddels
                    |> Radio.create
                        [ Radio.checked (nivaa == LavTilMiddels)
                        , Radio.onClick (UpdateRadio tiltak (NivaaType LavTilMiddels))
                        ]
                ]
        )


stedGroup : Tiltak -> TiltakStates -> Html Msg
stedGroup ((Tiltak object) as tiltak) tiltakStates =
    let
        sted =
            Focus.get object.stedFocus tiltakStates
    in
    Form.group []
        ([ Form.label [ for "stedRadios" ] [ Html.strong [] [ text "Sted" ] ] ]
            ++ Radio.radioList "stedRadios"
                [ Radio.create
                    [ Radio.checked (sted == Storby)
                    , Radio.onClick (UpdateRadio tiltak (StedType Storby))
                    ]
                    "Storby"
                , Radio.create
                    [ Radio.checked (sted == LitenBy)
                    , Radio.onClick (UpdateRadio tiltak (StedType LitenBy))
                    ]
                    "Liten by"
                , Radio.create
                    [ Radio.checked (sted == Spredtbygd)
                    , Radio.onClick (UpdateRadio tiltak (StedType Spredtbygd))
                    ]
                    "Spredtbygd"
                ]
        )


tiltakForm : Tiltak -> TiltakStates -> Html Msg
tiltakForm tiltak tiltakStates =
    let
        formGroups =
            sendTo tiltak .fields |> List.map (fieldView tiltak tiltakStates)
    in
    Form.form []
        ([ nivaaGroup tiltak tiltakStates
         , stedGroup tiltak tiltakStates
         ]
            ++ formGroups
        )

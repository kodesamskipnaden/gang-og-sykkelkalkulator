module Main exposing (main)

import Bootstrap.Accordion as Accordion
import Field exposing (Field, FieldSpec(..))
import Focus exposing ((=>))
import FormattedValue exposing (value)
import Group
import Models exposing (..)
import Msgs exposing (Msg(..), RadioValue(..))
import Navigation exposing (Location)
import Ports
import Tiltak exposing (Tiltak(..), TiltakStates, sendTo)
import TiltakAndGroupData
import TiltakCharting exposing (GraphState(..))
import UrlParser exposing ((</>))
import Views exposing (view)


titleFromPage : Page -> String
titleFromPage page =
    let
        appName =
            "Gang- og sykkel-vei kalkulator"
    in
    case page of
        Home ->
            appName

        NotFound ->
            "Ugyldig side"

        GroupPage group ->
            Group.groupTitle group ++ " - " ++ appName


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { page = pageFromLocation location
            , accordionState = Accordion.initialState
            , tiltakStates = TiltakAndGroupData.initialTiltakStates
            , chartIds = []
            }

        dummyModel =
            { model | page = NotFound }
    in
    -- if we had more than one cmd, use Cmd.batch : List Cmd -> Cmd
    ( model
    , Cmd.batch
        [ destroyAndCreateCharts dummyModel model
        , Ports.setTitle (titleFromPage model.page)
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Accordion.subscriptions model.accordionState AccordionMsg
        , Ports.charts ChartsChanged
        ]


updateRadio :
    Model
    -> Tiltak
    -> RadioValue
    -> ( Model, Cmd msg )
updateRadio ({ tiltakStates } as model) ((Tiltak object) as tiltak) radioValue =
    let
        newTiltakStates =
            model.tiltakStates
                |> (case radioValue of
                        NivaaType nivaa ->
                            Focus.set object.nivaaFocus nivaa

                        StedType sted ->
                            Focus.set object.stedFocus sted
                   )
    in
    ( { model | tiltakStates = newTiltakStates }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                newModel =
                    { model | page = pageFromLocation location }
            in
            ( newModel
            , Cmd.batch
                [ destroyAndCreateCharts model newModel
                , Ports.setTitle (titleFromPage newModel.page)
                ]
            )

        AccordionMsg state ->
            ( { model | accordionState = state }, Cmd.none )

        UpdateField tiltak field stringValue ->
            updateField model tiltak field stringValue

        UpdateRadio tiltak radioValue ->
            updateRadio model tiltak radioValue

        FieldBlur field ->
            ( { model | tiltakStates = field.beDisplayMode model.tiltakStates }
            , Cmd.none
            )

        FieldFocus field ->
            ( { model | tiltakStates = field.beEditMode model.tiltakStates }
            , Cmd.none
            )

        ChartsChanged chartIds ->
            ( { model | chartIds = chartIds }, Cmd.none )

        UpdateFieldToGraph tiltak field ->
            updateFieldToGraph tiltak field model


destroyAndCreateCharts : Model -> Model -> Cmd msg
destroyAndCreateCharts oldModel newModel =
    Cmd.batch
        [ case oldModel.page of
            GroupPage group ->
                groupDestroyCharts group oldModel

            _ ->
                Cmd.none
        , case newModel.page of
            GroupPage group ->
                groupCreateCharts group newModel

            _ ->
                Cmd.none
        ]


groupDestroyCharts : Group -> Model -> Cmd msg
groupDestroyCharts group model =
    TiltakAndGroupData.tiltakForGroup group
        |> List.filter
            (\tiltak ->
                TiltakCharting.graphState tiltak model.tiltakStates == GraphOn
            )
        |> List.map (\tiltak -> sendTo tiltak .graphId)
        |> List.map Ports.destroyC3
        |> Cmd.batch


groupCreateCharts : Group -> Model -> Cmd msg
groupCreateCharts group model =
    TiltakAndGroupData.tiltakForGroup group
        |> List.filter
            (\tiltak ->
                TiltakCharting.graphState tiltak model.tiltakStates == GraphOn
            )
        |> List.map (\tiltak -> TiltakCharting.chartRecord tiltak model.tiltakStates)
        |> List.map Ports.generateC3
        |> Cmd.batch


pageFromLocation : Navigation.Location -> Page
pageFromLocation location =
    decode location |> Maybe.withDefault NotFound


computeGraphCmd : Tiltak -> TiltakStates -> ( GraphState, GraphState ) -> Cmd msg
computeGraphCmd tiltak tiltakStates ( beforeState, afterState ) =
    let
        graphId =
            sendTo tiltak .graphId

        graphData =
            { domId = graphId
            , data = TiltakCharting.graphData tiltak tiltakStates
            , variableTitle =
                TiltakCharting.maybeFieldToGraph tiltak tiltakStates
                    |> Maybe.map .title
                    |> Maybe.withDefault "WAT!!!!"
            }
    in
    case ( beforeState, afterState ) of
        ( GraphOff, GraphOn ) ->
            Ports.generateC3 graphData

        ( GraphOff, GraphOff ) ->
            Cmd.none

        ( GraphOn, GraphOn ) ->
            -- generate command to update the graph
            Ports.updateC3 graphData

        ( GraphOn, GraphOff ) ->
            Ports.destroyC3 graphId


updateField : Model -> Tiltak -> Field -> String -> ( Model, Cmd Msg )
updateField model tiltak field stringValue =
    let
        maybeValue =
            let
                maybeFloat =
                    stringValue |> String.toFloat |> Result.toMaybe
            in
            case field.fieldSpec of
                PercentSpec ->
                    maybeFloat |> Maybe.map (\float -> float / 100)

                _ ->
                    maybeFloat

        updatePreferredToGraph preferredString =
            case
                TiltakCharting.maybeFieldToGraph
                    tiltak
                    model.tiltakStates
            of
                Just field ->
                    field.name

                Nothing ->
                    preferredString

        newTiltakStates =
            model.tiltakStates
                |> Focus.set (field.focus => value) maybeValue
                |> Focus.update
                    (Tiltak.getAttr tiltak .preferredToGraphFocus)
                    updatePreferredToGraph
    in
    updateGraphingState model tiltak newTiltakStates


updateGraphingState : Model -> Tiltak -> TiltakStates -> ( Model, Cmd msg )
updateGraphingState model tiltak newTiltakStates =
    let
        oldGraphState =
            TiltakCharting.graphState tiltak model.tiltakStates

        newGraphState =
            TiltakCharting.graphState tiltak newTiltakStates
    in
    ( { model
        | tiltakStates = newTiltakStates
      }
    , computeGraphCmd
        tiltak
        newTiltakStates
        ( oldGraphState
        , newGraphState
        )
    )


updateFieldToGraph : Tiltak -> Field -> Model -> ( Model, Cmd Msg )
updateFieldToGraph tiltak field model =
    let
        focus =
            Tiltak.getAttr tiltak .preferredToGraphFocus

        newTiltakStates =
            Focus.set
                focus
                field.name
                model.tiltakStates
    in
    updateGraphingState
        model
        tiltak
        newTiltakStates


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    let
        groupToParser group =
            UrlParser.map (GroupPage group) <|
                UrlParser.s <|
                    Group.groupPathSansHash group
    in
    TiltakAndGroupData.alleTyper
        |> List.map groupToParser
        |> List.append
            [ UrlParser.map Home UrlParser.top
            ]
        |> UrlParser.oneOf

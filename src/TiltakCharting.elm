module TiltakCharting exposing (..)

import Charting
import Field exposing (FieldSpec(..))
import Focus
import FormattedValue exposing (value)
import Tiltak exposing (Tiltak, sendTo)


type GraphState
    = GraphOff
    | GraphOn


chartRecord tiltak tiltakStates =
    { domId = sendTo tiltak .graphId
    , data = graphData tiltak tiltakStates
    , variableTitle =
        maybeFieldToGraph tiltak tiltakStates
            |> Maybe.map .title
            |> Maybe.withDefault "WAT!!!!"
    }


maybeFieldToGraph tiltak state =
    let
        filterFunc field =
            case field.value state of
                Just _ ->
                    False

                Nothing ->
                    True

        nothingFields =
            sendTo tiltak .fields
                |> List.filter filterFunc
    in
    case nothingFields of
        [ head ] ->
            Just head

        [] ->
            sendTo tiltak .preferredField state

        _ ->
            Nothing


possibleFieldsToGraph tiltak state =
    let
        filterFunc field =
            case field.value state of
                Just _ ->
                    False

                Nothing ->
                    True

        nothingFields =
            sendTo tiltak .fields
                |> List.filter filterFunc

        maybeFieldToGraphName =
            maybeFieldToGraph tiltak state
                |> Maybe.map .name
    in
    case nothingFields of
        [] ->
            sendTo tiltak .fields
                |> (case maybeFieldToGraphName of
                        Just name ->
                            List.filter (\field -> field.name /= name)

                        Nothing ->
                            identity
                   )

        _ ->
            []


graphState tiltak state =
    maybeFieldToGraph tiltak state
        |> Maybe.map (always GraphOn)
        |> Maybe.withDefault GraphOff


graphDataForField tiltak state field =
    let
        stateFrom x =
            Focus.set (Focus.join field.focus value) (Just x) state

        nettoNytte x =
            sendTo tiltak .nettoNytteInklOverfoert (stateFrom x)

        generateData x =
            nettoNytte x
                |> Maybe.map (\y -> ( x, y ))

        sampleFunc x =
            case nettoNytte x of
                Just value ->
                    value

                Nothing ->
                    Debug.todo "nettoNytte gave Nothing"

        stepSize =
            case field.fieldSpec of
                PercentSpec ->
                    0.1

                FloatSpec { stepSize } ->
                    stepSize |> toFloat

                IntSpec { stepSize } ->
                    stepSize |> toFloat
    in
    Charting.samples stepSize sampleFunc
        |> List.map generateData
        |> List.filterMap identity


graphData tiltak state =
    let
        maybeField =
            maybeFieldToGraph tiltak state
    in
    case maybeField of
        Nothing ->
            []

        Just field ->
            graphDataForField tiltak state field

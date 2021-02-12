module TiltakChartingTest exposing (suite)

import Expect exposing (Expectation)
import FormattedValue exposing (formattedValue)
import Test exposing (Test, describe, only, skip, test)
import Tiltak exposing (TiltakAccessor, sendTo)
import Tiltak.LEDLys as LEDLys exposing (tiltak)
import TiltakAndGroupData
import TiltakCharting


suite : Test
suite =
    skip <|
        describe "TiltakCharting"
            (let
                initialState =
                    TiltakAndGroupData.initialTiltakStates

                state =
                    { initialState
                        | ledLys =
                            { installationCost = Just 100 |> formattedValue
                            , yearlyMaintenance = Just 200 |> formattedValue
                            , sykkelturerPerYear = Nothing |> formattedValue

                            --                            , beleggForbiPassasjererPerBuss = Just 20 |> formattedValue
                            --                            , yearlyTidsbesparelseMinutter = Just 30 |> formattedValue
                            , lengdeSykkelveiKm = Just 1 |> formattedValue
                            , preferredToGraph = "sykkelturerPerYear"
                            }
                    }

                maybeField =
                    sendTo tiltak .fields |> List.filter (\field -> field.name == "sykkelturerPerYear") |> List.head

                sykkelturerPerYear =
                    case maybeField of
                        Just value ->
                            value

                        Nothing ->
                            Debug.crash "TODO"
             in
             [ test "graphFor" <|
                \() ->
                    state
                        |> TiltakCharting.graphData tiltak
                        |> Expect.equal
                            [ ( 0, 17920.795510263626 )
                            , ( 50, 19089.54839671369 )
                            , ( 100, 20258.301283163753 )
                            , ( 150, 21427.054169613817 )
                            , ( 200, 22595.80705606388 )
                            , ( 250, 23764.559942513944 )
                            , ( 300, 24933.312828964008 )
                            , ( 350, 26102.065715414064 )
                            , ( 400, 27270.818601864128 )
                            ]
             , describe "maybeFieldToGraph"
                [ test "sykkelturerPerYear" <|
                    \() ->
                        TiltakCharting.maybeFieldToGraph tiltak state
                            |> Expect.equal (Just sykkelturerPerYear)
                , test "all fields are valid chooses the last chosen field" <|
                    \() ->
                        let
                            ledLysFelt =
                                state.ledLys

                            modifiedState =
                                { state
                                    | ledLys =
                                        { ledLysFelt
                                            | sykkelturerPerYear = Just 10 |> formattedValue
                                        }
                                }
                        in
                        TiltakCharting.maybeFieldToGraph tiltak modifiedState
                            |> Expect.equal (Just sykkelturerPerYear)
                , test "two fields are invalid" <|
                    \() ->
                        let
                            ledLysFelt =
                                state.ledLys

                            modifiedState =
                                { state
                                    | ledLys =
                                        { ledLysFelt
                                            | yearlyMaintenance =
                                                Nothing
                                                    |> formattedValue
                                        }
                                }
                        in
                        TiltakCharting.maybeFieldToGraph tiltak modifiedState
                            |> Expect.equal Nothing
                ]
             ]
            )

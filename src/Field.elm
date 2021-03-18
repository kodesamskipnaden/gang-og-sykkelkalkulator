module Field exposing (..)

import Focus exposing ((=>), Focus)
import FormattedValue exposing (Editable(..), FormattedValue, state, value)
import Models
import TiltakStates exposing (TiltakStates)


type alias Field =
    Models.Field


type alias SimpleField =
    { name : String
    , title : String
    , placeholder : String
    , stepSize : Float

    -- focus for TiltakStates to fields value
    , focus : Focus TiltakStates (FormattedValue Float)
    }


type alias FieldValue =
    String


transformToFields : List SimpleField -> List Field
transformToFields fieldDefinitions =
    let
        toRealField simpleField =
            { name = simpleField.name
            , title = simpleField.title
            , placeholder = simpleField.placeholder
            , stepSize = simpleField.stepSize
            , focus = simpleField.focus
            , isEditable =
                \tiltakStates ->
                    case Focus.get (simpleField.focus => state) tiltakStates of
                        Edit ->
                            True

                        Display ->
                            False
            , beDisplayMode =
                \tiltakStates ->
                    Focus.set (simpleField.focus => state) Display tiltakStates
            , beEditMode =
                \tiltakStates ->
                    Focus.set (simpleField.focus => state) Edit tiltakStates
            , value = Focus.get (simpleField.focus => value)
            }
    in
    fieldDefinitions
        |> List.map toRealField


yearlyMaintenancePlaceholder : String
yearlyMaintenancePlaceholder =
    "Årlige (økninger i) kostnader til drift og vedlikehold som knytter seg til dette tiltaket"


installationCostSimpleField specificState =
    { name = "installationCost"
    , title = "Installasjonskostnad"
    , placeholder = "Kostnaden ved å installere tiltaket en gang, kroner"
    , focus = specificState => FormattedValue.installationCost
    , stepSize = 50000
    }


yearlyMaintenanceSimpleField specificState =
    { name = "yearlyMaintenance"
    , title = "Økte årlige drifts- og vedlikeholdskostnader"
    , placeholder = yearlyMaintenancePlaceholder
    , focus = specificState => FormattedValue.yearlyMaintenance
    , stepSize = 5000
    }


lengdeVeiKmSimpleField specificState =
    { name = "lengdeVeiKm"
    , title = "Veilengde i kilometer"
    , placeholder = "Lengde vei (km)"
    , focus = specificState => FormattedValue.lengdeVeiKm
    , stepSize = 5
    }


sykkelturerPerYearSimpleField specificState =
    { name = "sykkelturerPerYear"
    , title = "Antall sykkelturer per år"
    , placeholder = "Turer på mørke tider som får nytte av tiltaket"
    , focus = specificState => FormattedValue.sykkelturerPerYear
    , stepSize = 50
    }


gangturerPerYearSimpleField specificState =
    { name = "gangturerPerYear"
    , title = "Antall gangturer per år"
    , placeholder = "Turer på mørke tider som får nytte av tiltaket"
    , focus = specificState => FormattedValue.gangturerPerYear
    , stepSize = 50
    }

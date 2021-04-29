module Field exposing (..)

import Focus exposing ((=>), Focus)
import FormattedValue exposing (Editable(..), FormattedValue, state, value)
import Models exposing (FieldSpec(..))
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
            , fieldSpec = FloatSpec { stepSize = simpleField.stepSize }
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


lengdeVeiKmSimpleField :
    Focus TiltakStates { b | lengdeVeiKm : FormattedValue Float }
    -> SimpleField
lengdeVeiKmSimpleField specificState =
    { name = "lengdeVeiKm"
    , title = "Veilengde i kilometer"
    , placeholder = "Lengde vei (km)"
    , focus = specificState => FormattedValue.lengdeVeiKm
    , stepSize = 5
    }


sykkelturerPerYearSimpleField :
    Focus TiltakStates { specificState | sykkelturerPerYear : FormattedValue Float }
    -> SimpleField
sykkelturerPerYearSimpleField specificState =
    { name = "sykkelturerPerYear"
    , title = "Antall sykkelturer per år"
    , placeholder = "Turer på mørke tider som får nytte av tiltaket"
    , focus = specificState => FormattedValue.sykkelturerPerYear
    , stepSize = 50
    }


gangturerPerYearSimpleField :
    Focus TiltakStates { specificState | gangturerPerYear : FormattedValue Float }
    -> SimpleField
gangturerPerYearSimpleField specificState =
    { name = "gangturerPerYear"
    , title = "Antall gangturer per år"
    , placeholder = "Turer på mørke tider som får nytte av tiltaket"
    , focus = specificState => FormattedValue.gangturerPerYear
    , stepSize = 50
    }

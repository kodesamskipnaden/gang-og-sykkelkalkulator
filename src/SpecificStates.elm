module SpecificStates exposing (..)

import FormattedValue exposing (..)


type alias BasicState =
    { sykkelturerPerYear : FormattedValue Float
    , gangturerPerYear : FormattedValue Float
    , preferredToGraph : String
    }


type alias SimpleCommonPartial a =
    { a
        | yearlyMaintenance : FormattedValue Float
        , installationCost : FormattedValue Float
        , lengdeVeiKm : FormattedValue Float
    }


type alias SimpleCommonState =
    SimpleCommonPartial BasicState


type alias LEDLysState =
    SimpleCommonState


type alias GsB_GsAStatePartial a =
    { a
        | oppetidPercent : FormattedValue Float
    }


type alias GsB_GsAState =
    GsB_GsAStatePartial SimpleCommonState

module SpecificStates exposing (..)

import BasicState exposing (BasicState)
import FormattedValue exposing (..)


type alias SimpleCommonPartial a =
    { a
        | installationCost : FormattedValue Float
    }


type alias SimpleCommonState =
    SimpleCommonPartial BasicState


type alias YearlyMaintenancePartial a =
    { a | yearlyMaintenance : FormattedValue Float }


type alias LEDLysState =
    YearlyMaintenancePartial SimpleCommonState


type alias GsB_GsAStatePartial a =
    { a
        | oppetidPercent : FormattedValue Float
    }


type alias GsB_GsAState =
    GsB_GsAStatePartial SimpleCommonState

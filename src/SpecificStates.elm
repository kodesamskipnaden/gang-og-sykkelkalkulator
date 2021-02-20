module SpecificStates exposing (..)

import FormattedValue exposing (..)


type alias BasicState =
    { sykkelturerPerYear : FormattedValue Float
    , gangturerPerYear : FormattedValue Float
    , preferredToGraph : String
    }


type alias SuperSimpleCommonPartial a =
    { a | yearlyMaintenance : FormattedValue Float }


type alias SuperSimpleCommonState =
    SuperSimpleCommonPartial BasicState


type alias SimpleCommonPartial a =
    { a
        | installationCost : FormattedValue Float
    }


type alias SimpleCommonState =
    SimpleCommonPartial SuperSimpleCommonState


type alias OpphoeyetHoldeplassStatePartial a =
    { a
        | beleggForbiPassasjererPerBuss : FormattedValue Float
        , yearlyTidsbesparelseMinutter : FormattedValue Float
    }


type alias OpphoeyetHoldeplassState =
    OpphoeyetHoldeplassStatePartial SimpleCommonState


type alias LEDLysStatePartial a =
    { a
        | lengdeSykkelveiKm : FormattedValue Float
    }


type alias LEDLysState =
    LEDLysStatePartial SimpleCommonState


type alias KollektivPrioriteringLyskryssStatePartial a =
    { a
        | antallBilerForsinketPerAvgang : FormattedValue Float
        , forsinkelsePerBilSeconds : FormattedValue Float
        , antallPasserendeAvgangerPerYear : FormattedValue Float
    }


type alias KollektivPrioriteringLyskryssState =
    KollektivPrioriteringLyskryssStatePartial SimpleCommonState

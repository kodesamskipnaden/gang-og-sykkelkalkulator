module BasicState exposing (BasicState)

import FormattedValue exposing (FormattedValue)


type alias BasicState =
    { sykkelturerPerYear : FormattedValue Float
    , gangturerPerYear : FormattedValue Float
    , preferredToGraph : String
    , lengdeVeiKm : FormattedValue Float
    }

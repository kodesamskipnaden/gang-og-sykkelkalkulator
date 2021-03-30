module BasicState exposing (BasicState, Nivaa(..))

import FormattedValue exposing (FormattedValue)


type Nivaa
    = LavTilHoey
    | LavTilMiddels
    | MiddelsTilHoey


type alias BasicState =
    { sykkelturerPerYear : FormattedValue Float
    , gangturerPerYear : FormattedValue Float
    , preferredToGraph : String
    , lengdeVeiKm : FormattedValue Float
    , nivaa : Nivaa
    }

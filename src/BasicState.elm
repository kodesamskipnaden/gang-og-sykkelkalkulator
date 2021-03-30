module BasicState exposing (BasicState, Nivaa(..), Sted(..))

import FormattedValue exposing (FormattedValue)


type Nivaa
    = LavTilHoey
    | LavTilMiddels
    | MiddelsTilHoey


type Sted
    = Storby
    | LitenBy
    | Spredtbygd


type alias BasicState =
    { sykkelturerPerYear : FormattedValue Float
    , gangturerPerYear : FormattedValue Float
    , preferredToGraph : String
    , lengdeVeiKm : FormattedValue Float
    , nivaa : Nivaa
    , sted : Sted
    }

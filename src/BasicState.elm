module BasicState exposing (BasicState, BasicStatePartial, Nivaa(..), Sted(..))

import FormattedValue exposing (FormattedValue)


type alias NivaaForutsetninger =
    { etterspoerselsEffekt : Float
    , tidsbesparelseSyklendeMinutterPerKilometer : Float
    , tidsbesparelseGaaendeMinutterPerKilometer : Float
    , wtp : Float
    }


type Nivaa
    = LavTilHoey
    | LavTilMiddels
    | MiddelsTilHoey


type Sted
    = Storby
    | LitenBy
    | Spredtbygd


type alias BasicStatePartial a =
    { a
        | sykkelturerPerYear : FormattedValue Float
        , gangturerPerYear : FormattedValue Float
        , preferredToGraph : String
        , lengdeVeiKm : FormattedValue Float
        , nivaa : Nivaa
        , sted : Sted
    }


type alias BasicState =
    BasicStatePartial {}

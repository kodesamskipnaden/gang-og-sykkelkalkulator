module Group exposing (..)

import Models exposing (Group(..))


groupPath : Group -> String
groupPath tag =
    tag |> groupPathSansHash |> (++) "#"


groupPathSansHash : Group -> String
groupPathSansHash tag =
    case tag of
        GruppoA ->
            "GruppoA"
                |> String.toLower


groupTitle : Group -> String
groupTitle tag =
    case tag of
        GruppoA ->
            "Alle tiltak"

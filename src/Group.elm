module Group exposing (..)

import Models exposing (Group(..))


groupPath : Group -> String
groupPath tag =
    tag |> groupPathSansHash |> (++) "#"


groupPathSansHash : Group -> String
groupPathSansHash tag =
    tag |> toString |> String.toLower


groupTitle : Group -> String
groupTitle tag =
    case tag of
        Vedlikehold ->
            "Fra GsB (brøyt og strø) til GsA (sop og salt / barvegsstandard)"

        Belysning ->
            "Fra ingen/eldre belysning til LED-belysning"



-- _ ->
--     tag |> toString

module Msgs exposing (..)

import Bootstrap.Accordion as Accordion
import Field exposing (Field)
import Navigation exposing (Location)
import Tiltak exposing (Tiltak)


type Msg
    = UrlChange Location
    | AccordionMsg Accordion.State
    | UpdateField Tiltak Field String
    | ChartsChanged (List String)
    | FieldBlur Field
    | FieldFocus Field
    | UpdateFieldToGraph Tiltak Field

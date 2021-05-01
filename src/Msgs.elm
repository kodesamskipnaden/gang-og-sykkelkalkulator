module Msgs exposing (..)

import BasicState exposing (Nivaa, Sted)
import Bootstrap.Accordion as Accordion
import Browser exposing (UrlRequest)
import Field exposing (Field)
import Models exposing (Page)
import Tiltak exposing (Tiltak)


type RadioValue
    = NivaaType Nivaa
    | StedType Sted


type Msg
    = Visit UrlRequest
    | NewRoute (Maybe Page)
    | AccordionMsg Accordion.State
    | UpdateField Tiltak Field String
    | UpdateRadio Tiltak RadioValue
    | ChartsChanged (List String)
    | FieldBlur Field
    | FieldFocus Field
    | UpdateFieldToGraph Tiltak Field

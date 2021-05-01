module Models exposing (..)

import Bootstrap.Accordion as Accordion
import Browser.Navigation as Navigation
import TiltakStates exposing (TiltakStates)


type Group
    = GruppoA


type Page
    = Home
    | GroupPage Group
    | NotFound


type alias Model =
    { page : Page
    , navigationKey : Navigation.Key
    , accordionState : Accordion.State
    , tiltakStates : TiltakStates
    , chartIds : List String
    }

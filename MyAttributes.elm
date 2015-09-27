module MyAttributes where

import Json.Encode as Json
import Html exposing (Attribute)
import Html.Attributes exposing (property)

role : String -> Attribute
role name = property "role" (Json.string name)

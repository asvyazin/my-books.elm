module MyAttributes where

import Json.Encode as Json
import Html exposing (Attribute)
import Html.Attributes exposing (property, attribute)


renderBool : Bool -> String
renderBool b =
  if b
  then "true"
  else "false"


role : String -> Attribute
role name = attribute "role" name


dataBackdrop : String -> Attribute
dataBackdrop backdropType = attribute "data-backdrop" backdropType


dataKeyboard : String -> Attribute
dataKeyboard keyboardType = attribute "data-keyboard" keyboardType


dataRemote : String -> Attribute
dataRemote remoteType = attribute "data-remote" remoteType


dataDismiss : String -> Attribute
dataDismiss class = attribute "data-dismiss" class


dataShow : Bool -> Attribute
dataShow value = attribute "data-show" (renderBool value)


ariaHidden : Bool -> Attribute
ariaHidden value = attribute "aria-hidden" (renderBool value)

module MasterPage (view) where

import Html exposing (..)
import Html.Attributes as A
import Html.Shorthand exposing (..)


view : List Html -> Html
view children =
  div' { class = "container" } children

module MasterPage (view) where

import Html exposing (..)
import Html.Attributes as A
import Html.Shorthand exposing (..)


view : List Html -> Html
view children =
  let
    bootstrapCss = node "link" [A.href "/bower_components/bootstrap/dist/css/bootstrap.min.css", A.rel "stylesheet"] []
    bootstrapJs = node "script" [A.src "/bower_components/bootstrap/dist/js/bootstrap.min.js"] []
  in 
    div' { class = "container" }
           (bootstrapCss :: bootstrapJs :: children)

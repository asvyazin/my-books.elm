module MasterPage (view) where

import Html exposing (..)
import Html.Attributes as A
import Html.Shorthand exposing (..)


view : List Html -> Html
view children =
  let
    bootstrapCss = node "link" [A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css", A.rel "stylesheet"] []
  in 
    div' { class = "container" }
           (bootstrapCss :: children)

module Header (Model, view) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Shorthand exposing (..)
import MyAttributes


type alias Model = { title : String, userName : String }


navbarDefault_ : List Html -> Html
navbarDefault_ = nav [ class "navbar navbar-default", MyAttributes.role "navigation" ]
                 
                 
view : Model -> Html
view model = navbarDefault_
  [ div' { class = "navbar-header navbar-brand" }
    [text model.title]
  , ul' { class = "nav navbar-nav" }
    [ li_
      [ div' { class = "navbar-text" }
        [text "Get new access token"]
      ]
    ]
  , ul' { class = "nav navbar-nav navbar-right" }
    [ li_
      [ div' { class = "navbar-text" }
        [text model.userName]
      ]
    ]
  ]

module Login where

import Effects exposing (Never)
import Html exposing (Html)
import Html.Attributes as A
import Html.Shorthand exposing (..)
import Http
import StartApp exposing (..)
import Task exposing (Task)

import Header
import MasterPage


type alias ButtonModel =
  { clientId : String
  , scope : String
  , redirectURI : String
  }


loginButton : ButtonModel -> Html
loginButton model =
  a'
    {
      class = "btn btn-default btn-lg",
      href = loginUrl model
    }
    [ glyphiconCloud, Html.text " Go to OneDrive" ]


glyphiconCloud : Html
glyphiconCloud =
  span' { class = "glyphicon glyphicon-cloud" } []


loginUrl : ButtonModel -> String
loginUrl model =
  Http.url
        "https://login.live.com/oauth20_authorize.srf"
        [ ("client_id", model.clientId)
        , ("scope", model.scope)
        , ("redirect_uri", model.redirectURI)
        , ("response_type", "token")
        ]


loginPage : Header.Model -> Html
loginPage model =
  let
    buttonContainer =
      div'
      { class = "col-md-offset-5 col-md-2" }
      [
       loginButton
       {
         clientId = "000000004816D42C",
         redirectURI = "http://localhost:8000/redirect.html",
         scope = "wl.signin onedrive.readonly"
       }
      ]
  in MasterPage.view [Header.view model, buttonContainer]


app =
  start { init = (Header.init "Title")
        , update = Header.update
        , view = view
        , inputs = []
        }


main : Signal Html
main =
  app.html


view : Signal.Address Header.Action -> Header.Model -> Html
view _ model =
  loginPage model


port tasks : Signal (Task Never ())
port tasks =
  app.tasks

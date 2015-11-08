module Header (Model, Action(..), init, update, view) where


import Effects exposing (Effects)
import EffectsExtensions as Effects
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Shorthand exposing (..)
import Http
import Json.Decode as Json
import MaybeExtensions as Maybe
import MyAttributes
import Task


type alias Model =
  { title : String
  , userName : Maybe String
  , error : Maybe String }


type Action =
  AccessTokenReceived (Maybe String) |
  UserNameReceived String |
  ErrorReceived String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AccessTokenReceived accessToken ->
      (model, getUserName accessToken)
    UserNameReceived user ->
      Effects.noFx {model | userName <- Just user}
    ErrorReceived error ->
      Effects.noFx {model | error <- Just error}


navbarDefault_ : List Html -> Html
navbarDefault_ =
  nav [ class "navbar navbar-default", MyAttributes.role "navigation" ]
                 
                 
view : Model -> Html
view model =
  let
    brand =
      div' { class = "navbar-header navbar-brand" } [ text model.title ]
           
    loginLink =
      ul' { class = "nav navbar-nav" }
            [ li_ [ a_ "/login.html" "Get new access token" ] ]

    userNameOrErrorElems =
      Maybe.filterJust
             [ Maybe.map renderUserName model.userName
             , Maybe.map renderError model.error ]
            
    userNameOrError =
      ul' { class = "nav navbar-nav navbar-right" } userNameOrErrorElems 

  in
    navbarDefault_ [ brand
                   , loginLink
                   , userNameOrError
                   ]


renderUserName : String -> Html
renderUserName userName =
  li_ [ div' { class = "navbar-text" } [text userName] ]


renderError : String -> Html
renderError error =
  li_ [ div' { class = "alert alert-danger" } [text error] ]


init : String -> (Model, Effects Action)
init title =
  (Model title Nothing Nothing, Effects.none)


getUserName : Maybe String -> Effects Action
getUserName accessToken =
  Maybe.withDefault Effects.none (Maybe.map doGetUserName accessToken)
             

doGetUserName : String -> Effects Action
doGetUserName accessToken =
  Http.get (Json.at ["name"] Json.string) (userNameUrl accessToken)
    |> Task.toResult
    |> Task.map (Result.formatError convertError >> toAction)
    |> Effects.task


convertError : Http.Error -> String
convertError err =
  case err of
    Http.BadResponse _ msg -> msg
    _ -> "Unknown error"


toAction : Result String String -> Action
toAction result =
  case result of
    Ok name -> UserNameReceived name
    Err error -> ErrorReceived error
                         

userNameUrl : String -> String
userNameUrl accessToken =
  Http.url "https://apis.live.net/v5.0/me" [("access_token", accessToken)]

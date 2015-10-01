module Header (Model, Action, init, update, view) where


import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Shorthand exposing (..)
import Http
import Json.Decode as Json
import MyAttributes
import Task


type alias Model =
  { title : String
  , userName : Maybe String
  , error : Maybe String }


type Action =
  UserNameReceived String |
  ErrorReceived String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UserNameReceived user -> ({model | userName <- Just user}, Effects.none)
    ErrorReceived error -> ({model | error <- Just error}, Effects.none)


navbarDefault_ : List Html -> Html
navbarDefault_ = nav [ class "navbar navbar-default", MyAttributes.role "navigation" ]


maybeToList : Maybe a -> List a
maybeToList xx =
  case xx of
    Nothing -> []
    Just x -> [x]
                 
                 
view : Model -> Html
view model = navbarDefault_
  [ div' { class = "navbar-header navbar-brand" }
    [text model.title]
  , ul' { class = "nav navbar-nav" }
    [
     li_
         [ a_ "/Login.elm" "Get new access token" ]
    ]
  , ul' { class = "nav navbar-nav navbar-right" }
    (List.append
      (maybeToList (Maybe.map renderUserName model.userName))
      (maybeToList (Maybe.map renderError model.error)))
  ]


renderUserName : String -> Html
renderUserName userName = li_ [ div' { class = "navbar-text" } [text userName] ]


renderError : String -> Html
renderError error = li_ [ div' { class = "alert alert-danger" } [text error] ]


init : String -> String -> (Model, Effects Action)
init title accessToken = (Model title Nothing Nothing, getUserName accessToken)


getUserName : String -> Effects Action
getUserName accessToken =
  Http.get (Json.at ["name"] Json.string) (userNameUrl accessToken)
    |> Task.toResult
    |> Task.map (Result.formatError convertError)
    |> Task.map toAction
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
userNameUrl accessToken = Http.url "https://apis.live.net/v5.0/me" [("access_token", accessToken)]

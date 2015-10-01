module Main where

import Effects exposing (Never)
import Html
import MasterPage
import StartApp exposing (start)
import Task
import Header


app = start { init = (Header.init "Title" "123"), update = Header.update, view = view, inputs = [] }


main : Signal Html.Html
main =
  app.html


view : Signal.Address Header.Action -> Header.Model -> Html.Html
view _ model =
  MasterPage.view [Header.view model]


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

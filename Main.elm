module Main where

import Effects exposing (Never, Effects)
import EffectsExtensions as Effects
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as A
import ListExtensions as L
import MasterPage
import MaybeExtensions as M
import StartApp exposing (start)
import Task exposing (Task)
import Header
import OneDriveDirectoryChooserModal


type alias DirectoryChooserModel =
  { modal : OneDriveDirectoryChooserModal.Model
  , directory : Maybe String
  }


type alias Model =
  { directoryChooser : Maybe DirectoryChooserModel
  , header : Header.Model
  , accessToken : Maybe String
  }


type Action
  = HeaderAction Header.Action
  | DirectoryChooserAction OneDriveDirectoryChooserModal.Action
  | AccessTokenReceived (Maybe String)
  | ShowDirectoryChooser
  | ShowDirectoryChooserComplete
  | HideDirectoryChooser
  | HideDirectoryChooserComplete


port accessToken : Signal (Maybe String)


app =
  let
    accessTokenInput =
      Signal.map AccessTokenReceived accessToken
  in
    start { init = init, update = update, view = view, inputs = [accessTokenInput] }


init : (Model, Effects Action)
init =
  let
    (headerModel, headerEffects) = Header.init "MyBooks"
    model =
      { directoryChooser = Nothing
      , header = headerModel
      , accessToken = Nothing
      }
  in
    (model, Effects.map HeaderAction headerEffects)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    HeaderAction headerAction ->
      let
        (newHeaderModel, headerEffects) = Header.update headerAction model.header
      in
        ({ model | header <- newHeaderModel }, Effects.map HeaderAction headerEffects)

    DirectoryChooserAction chooserAction ->
      let
        updateChooser chooser =
          let
            (newModal, eff) = OneDriveDirectoryChooserModal.update chooserAction chooser.modal
          in
            (Just {chooser | modal <- newModal}, eff)

        (newChooser, directoryChooserEffects) =
          Maybe.map updateChooser model.directoryChooser
               |> Maybe.withDefault (model.directoryChooser, Effects.none)
      in
        ({ model | directoryChooser <- newChooser }, Effects.map DirectoryChooserAction directoryChooserEffects)

    AccessTokenReceived maybeAccessToken ->
      Maybe.map (\token ->
                   let
                     (headerModel, headerEffects) =
                       Header.update (Header.AccessTokenReceived (Just token)) model.header

                     (chooser, chooserEffects) =
                       OneDriveDirectoryChooserModal.init token

                     directoryChooser =
                       { modal = chooser
                       , directory = Nothing
                       }
                   in
                     ({ model | header <- headerModel, directoryChooser <- Just directoryChooser, accessToken <- Just token }
                     , Effects.batch
                     [ Effects.map HeaderAction headerEffects
                     , Effects.map DirectoryChooserAction chooserEffects ])
                ) maybeAccessToken
        |> Maybe.withDefault (model, Effects.none)

    ShowDirectoryChooser ->
      (model, OneDriveDirectoryChooserModal.show
         |> Effects.task
         |> Effects.map (always ShowDirectoryChooserComplete))

    ShowDirectoryChooserComplete ->
      (model, Effects.none)

    HideDirectoryChooser -> 
      (model, OneDriveDirectoryChooserModal.hide
         |> Effects.task
         |> Effects.map (always HideDirectoryChooserComplete))

    HideDirectoryChooserComplete ->
      (model, Effects.none)


view : Signal.Address Action -> Model -> Html
view address model =
  let
    maybeChooserHtml =
      Maybe.map (viewChooser address) model.directoryChooser

    folderIcon =
      Html.span [A.class "glyphicon glyphicon-folder-close"] []

    button =
      Html.button [A.class "btn btn-default btn-lg", A.onClick address ShowDirectoryChooser] [folderIcon, Html.text " Choose directory"]

    linkHtml =
      Html.div [A.class "col-md-offset-5 col-md-2"] [button]

    elems =
      M.filterJust [maybeChooserHtml, Just linkHtml]

  in
    MasterPage.view (Header.view model.header :: elems)


viewChooser : Signal.Address Action -> DirectoryChooserModel -> Html
viewChooser address model =
  OneDriveDirectoryChooserModal.view (Signal.forwardTo address DirectoryChooserAction) model.modal


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks


port showOneDriveDirectoryChooserModal : Signal String
port showOneDriveDirectoryChooserModal =
  OneDriveDirectoryChooserModal.showSignal


port hideOneDriveDirectoryChooserModal : Signal String
port hideOneDriveDirectoryChooserModal =
  OneDriveDirectoryChooserModal.hideSignal


port saveFolder : Signal (Maybe String)
port saveFolder =
  OneDriveDirectoryChooserModal.saveFolderSignal

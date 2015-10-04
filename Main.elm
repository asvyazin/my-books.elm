module Main where

import Effects exposing (Never, Effects)
import Html exposing (Html)
import Html.Shorthand exposing (..)
import MasterPage
import String
import StartApp exposing (start)
import Task exposing (Task)
import Header
import OneDriveApi
import TreeView exposing (TreeModel)


type OneDriveItemModel
  = Loading
  | OneDriveFileModel
    { name : String }
  | OneDriveFolderModel
    { name : String
    , childrenCount : Int
    }


type alias OneDriveTreeModel = TreeView.TreeModel OneDriveItemModel String
type alias OneDriveTreeAction = TreeView.Action OneDriveItemModel String
type alias OneDriveTreeItemModel = TreeView.TreeItemModel OneDriveItemModel String


type alias Model =
  { tree : Maybe OneDriveTreeModel
  , header : Header.Model
  }


type Action
  = HeaderAction Header.Action
  | TreeAction OneDriveTreeAction
  | AccessTokenReceived (Maybe String)
  | OneDriveTreeReceived (Result String OneDriveTreeModel)


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
      { tree = Nothing
      , header = headerModel
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
        
    TreeAction treeAction ->
      Maybe.map (TreeView.update treeAction) model.tree
        |> Maybe.map (\ (treeModel, treeEffects) ->
                        ({ model | tree <- Just treeModel }, Effects.map TreeAction treeEffects) )
        |> Maybe.withDefault (model, Effects.none)
           
    AccessTokenReceived maybeAccessToken ->
      Maybe.map (\token ->
                   let
                     (headerModel, headerEffects) = Header.update (Header.AccessTokenReceived (Just token)) model.header
                   in
                     ({ model | header <- headerModel }
                     , Effects.batch
                     [ Effects.map HeaderAction headerEffects
                     , getOneDriveTree token ])
                ) maybeAccessToken
        |> Maybe.withDefault (model, Effects.none)

    OneDriveTreeReceived result ->
      Result.toMaybe result
        |> Maybe.map (\tree -> ({ model | tree <- Just tree }, Effects.none))
        |> Maybe.withDefault (model, Effects.none)


getOneDriveTree : String -> Effects Action
getOneDriveTree token =
  doOneDriveLoadChildren token "/"
    |> Task.map (Result.map (treeModel token "/"))
    |> Task.map OneDriveTreeReceived
    |> Effects.task


treeModel : String -> String -> List OneDriveTreeItemModel -> OneDriveTreeModel
treeModel token path elements =
  let
    loadChildren path =
      doOneDriveLoadChildren token path
        |> Task.map Result.toMaybe
        |> Task.map (Maybe.withDefault [])
  in
    { title = path
    , elements = elements
    , viewElement = viewOneDriveItemModel
    , loadingPlaceholder = Loading
    , loadChildren = loadChildren
    }
  

viewOneDriveItemModel : OneDriveItemModel -> Html
viewOneDriveItemModel model =
  case model of
    Loading ->
      Html.text "Loading..."
    OneDriveFileModel x ->
      Html.text x.name
    OneDriveFolderModel x ->
      Html.text x.name


doOneDriveLoadChildren : String -> String -> Task Never (Result String (List OneDriveTreeItemModel))
doOneDriveLoadChildren token path =
  OneDriveApi.oneDriveGetChildren token path
    |> Task.map (Result.map (List.map (convertOneDriveItem path)))


convertOneDriveItem : String -> OneDriveApi.Item -> OneDriveTreeItemModel
convertOneDriveItem parentPath item =
  let
    content =
      case item.folder of
        Nothing ->
          OneDriveFileModel
          { name = item.name
          }
        Just folder ->
          OneDriveFolderModel
          { name = item.name
          , childrenCount = folder.childCount
          }
        
    hasChildren = Maybe.map (always True) item.folder |> Maybe.withDefault False

    normalizedParentPath =
      if String.endsWith parentPath "/"
      then parentPath
      else parentPath ++ "/"
    
    path = normalizedParentPath ++ item.name
  in
    TreeView.TreeItemModel
              { content = content
              , glyphicon = Nothing
              , href = Nothing
              , hasChildren = hasChildren
              , expanded = False
              , children = Nothing
              , id = Just path
              }


view : Signal.Address Action -> Model -> Html
view address model =
  let
    treeHtml =
      model.tree
        |> Maybe.map (TreeView.view (Signal.forwardTo address TreeAction))
        |> Maybe.map (\x -> [x])
        |> Maybe.withDefault []
  in
    MasterPage.view (Header.view model.header :: treeHtml)


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks

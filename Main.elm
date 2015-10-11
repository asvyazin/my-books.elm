module Main where

import Effects exposing (Never, Effects)
import EffectsExtensions as Effects
import Html exposing (Html)
import Html.Attributes as A
import Html.Shorthand exposing (..)
import MasterPage
import String
import StartApp exposing (start)
import Task exposing (Task)
import Header
import OneDriveApi
import Tree
import TreeView exposing (TreeModel)


type OneDriveItemModel
  = Loading
  | OneDriveFileModel
    { name : String }
  | OneDriveFolderModel
    { name : String
    , childrenCount : Int
    , childrenLoaded : Bool
    }


type alias Id = Maybe String
type alias OneDriveTreeModel = TreeView.TreeModel OneDriveItemModel Id
type alias OneDriveTreeAction = TreeView.Action Id
type alias OneDriveTreeItemModel = TreeView.TreeItemModel OneDriveItemModel Id


type alias Model =
  { tree : Maybe OneDriveTreeModel
  , header : Header.Model
  , accessToken : Maybe String
  }


type Action
  = HeaderAction Header.Action
  | TreeAction OneDriveTreeAction
  | AccessTokenReceived (Maybe String)
  | OneDriveTreeReceived (Result String OneDriveTreeModel)
  | OneDriveTreeExpandItem Id
  | OneDriveSubtreeReceived Id (Result String (List OneDriveTreeItemModel))


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
        
    TreeAction treeAction ->
      Maybe.map (TreeView.update treeAction) model.tree
        |> Maybe.map (\ treeModel ->
                        ({ model | tree <- Just treeModel }, Effects.none) )
        |> Maybe.withDefault (model, Effects.none)
           
    AccessTokenReceived maybeAccessToken ->
      Maybe.map (\token ->
                   let
                     (headerModel, headerEffects) = Header.update (Header.AccessTokenReceived (Just token)) model.header
                   in
                     ({ model | header <- headerModel, accessToken <- Just token }
                     , Effects.batch
                     [ Effects.map HeaderAction headerEffects
                     , getOneDriveTree token ])
                ) maybeAccessToken
        |> Maybe.withDefault (model, Effects.none)

    OneDriveTreeReceived result ->
      Result.toMaybe result
        |> Maybe.map (\tree -> ({ model | tree <- Just tree }, Effects.none))
        |> Maybe.withDefault (model, Effects.none)

    OneDriveSubtreeReceived id result ->
      let
        processSubtree itemModels =
          Maybe.map (processForest itemModels) model.tree

        processForest itemModels forest =
          List.map (processTree itemModels) forest

        processTree itemModels (Tree.Tree tree) =
          let
            oldData = tree.data
          in
            if oldData.params.id /= id
            then
              Tree.Tree { tree | children <- List.map (processTree itemModels) tree.children}
            else
              let
                oldParams = oldData.params
                oldContent = oldParams.content
                newContent =
                  case oldContent of
                    OneDriveFolderModel f ->
                      OneDriveFolderModel { f | childrenLoaded <- True }
                    _ ->
                      oldContent
                newParams = { oldParams | content <- newContent }
                newData = { oldData | params <- newParams }
              in 
                Tree.Tree { tree | children <- List.map treeModelSingleton itemModels, data <- newData }

        newTree =
          Result.toMaybe result
            `Maybe.andThen` processSubtree
      in
        ({ model | tree <- newTree }, Effects.none)

    OneDriveTreeExpandItem id ->
      let
        -- processForestExpandItem : OneDriveTreeModel -> (OneDriveTreeModel, Effects Action)
        processForestExpandItem forest =
          Effects.mapM processTreeExpandItem forest

        processTreeExpandItem (Tree.Tree tree) =
          let
            oldData = tree.data
          in
            if
              | oldData.params.id /= id ->
                let
                  newChildrenEff = Effects.mapM processTreeExpandItem tree.children
                in
                  (Tree.Tree { tree | children <- fst newChildrenEff }, snd newChildrenEff)
              | oldData.expanded ->
                (Tree.Tree { tree | data <- { oldData | expanded <- False } }, Effects.none)
              | otherwise ->
                case oldData.params.content of
                  OneDriveFolderModel folderModel ->
                    let
                      newData = { oldData | expanded <- True }
                    in 
                      if folderModel.childrenLoaded
                      then
                        (Tree.Tree { tree | data <- newData }, Effects.none)
                      else
                        let
                          params =
                            { id = Nothing
                            , content = Loading
                            , glyphicon = Nothing
                            , href = Nothing
                            }

                          data =
                            TreeView.item params

                          t =
                            Tree.Tree { data = data, children = [] }
                        in 
                          (Tree.Tree { tree | children <- [ t ], data <- newData }, loadChildren model.accessToken id)
                      
                  _ ->
                    (Tree.Tree tree, Effects.none)

        -- newTreeEff : (OneDriveTreeModel, Effects Action)
        newTreeEff =
          Maybe.map processForestExpandItem model.tree
            |> Maybe.map (\ (x, y) -> (Just x, y) )
            |> Maybe.withDefault (model.tree, Effects.none)
      in
        ({ model | tree <- fst newTreeEff}, snd newTreeEff)


getOneDriveTree : String -> Effects Action
getOneDriveTree token =
  doOneDriveLoadChildren token "/"
    |> Task.map (Result.map treeModel)
    |> Task.map OneDriveTreeReceived
    |> Effects.task


treeModelSingleton : OneDriveTreeItemModel -> Tree.Tree OneDriveTreeItemModel
treeModelSingleton item =
  Tree.Tree { data = item, children = [] }


treeModel : List OneDriveTreeItemModel -> OneDriveTreeModel
treeModel items =
    List.map treeModelSingleton items


loadChildren : Maybe String -> Id -> Effects Action
loadChildren accessToken id =
  accessToken `Maybe.andThen` (\token -> Maybe.map (doLoadChildren token) id)
    |> Maybe.withDefault Effects.none


doLoadChildren : String -> String -> Effects Action
doLoadChildren accessToken path =
  doOneDriveLoadChildren accessToken path
    |> Task.map (OneDriveSubtreeReceived (Just path))
    |> Effects.task


doOneDriveLoadChildren : String -> String -> Task Never (Result String (List OneDriveTreeItemModel))
doOneDriveLoadChildren token path =
  OneDriveApi.oneDriveGetChildren token path
    |> Task.map (Result.map (List.map (convertOneDriveItem path)))


convertOneDriveItem : String -> OneDriveApi.Item -> OneDriveTreeItemModel
convertOneDriveItem parentPath item =
  let
    normalizedParentPath =
      if String.endsWith parentPath "/"
      then parentPath
      else parentPath ++ "/"
    
    path =
      normalizedParentPath ++ item.name

    id = Just path
  
  in 
    case item.folder of
      Nothing ->
        TreeView.item
                  { id = id
                  , content =
                    OneDriveFileModel
                    { name = item.name
                    }
                  , glyphicon = Nothing
                  , href = Nothing
                  }
      Just folder ->
        TreeView.folderItem
                  { id = id
                  , content =
                    OneDriveFolderModel
                    { name = item.name
                    , childrenCount = folder.childCount
                    , childrenLoaded = False
                    }
                  , glyphicon = Nothing
                  , href = Nothing
                  }


view : Signal.Address Action -> Model -> Html
view address model =
  let
    viewContext =
      { actions = (Signal.forwardTo address TreeAction)
      , expand = (Signal.forwardTo address OneDriveTreeExpandItem)
      , viewContent = viewOneDriveItemModel
      }
    
    treeHtml =
      model.tree
        |> Maybe.map (TreeView.view viewContext)
        |> Maybe.map (\x -> [x])
        |> Maybe.withDefault []
  in
    MasterPage.view (Header.view model.header :: treeHtml)
  

viewOneDriveItemModel : OneDriveItemModel -> Html
viewOneDriveItemModel model =
  case model of
    Loading ->
      Html.img [ A.class "center-block text-center", A.src "/images/ajax-loader.gif" ] []
    OneDriveFileModel x ->
      Html.text x.name
    OneDriveFolderModel x ->
      let
        badgeHtml = span' { class = "badge pull-right" } [Html.text (toString x.childrenCount)]
      in
        span_ [Html.text x.name, badgeHtml]


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks

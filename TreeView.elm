module TreeView where

import Effects exposing (Effects, Never)
import Html
import Html.Shorthand exposing (..)
import Maybe exposing (andThen)
import Task exposing (Task)


type Action a b
  = ToggleExpanded b
  | ChildrenLoaded b (List (TreeItemModel a b))


type alias TreeModel a b =
  { title : String
  , elements : List (TreeItemModel a b)
  , viewElement : a -> Html.Html
  , loadingPlaceholder : a
  , loadChildren : b -> Task Never (List (TreeItemModel a b))
  }


type TreeItemModel a b = TreeItemModel
  { content : a
  , glyphicon : Maybe String
  , href : Maybe String
  , hasChildren : Bool
  , expanded : Bool
  , children : Maybe (List (TreeItemModel a b))
  , id : Maybe b
  }


loadingPlaceholderTreeItemModel : a -> TreeItemModel a b
loadingPlaceholderTreeItemModel placeholder =
  TreeItemModel
  { content = placeholder
  , glyphicon = Nothing
  , href = Nothing
  , hasChildren = False
  , expanded = False
  , children = Nothing
  , id = Nothing
  }


glyphicon_ : String -> Html.Html
glyphicon_ icon = span' { class = "glyphicon glyphicon-" ++ icon } []


view : Signal.Address (Action a b) -> TreeModel a b -> Html.Html
view address model =
  let
    -- viewTreeItem : Int -> TreeItemModel a -> List Html.Html
    viewTreeItem indent (TreeItemModel x) =
      let
        indentElems =
          List.repeat indent (span' { class = "indent" } [])

        folderGlyphiconClass =
          if not x.hasChildren
          then Nothing
          else
            if x.expanded
            then Just "minus"
            else Just "plus"

        folderGlyphicon =
          Maybe.withDefault [] (folderGlyphiconClass `andThen` renderFolderGlyphicon)

        renderFolderGlyphicon c =
          let
            doRenderFolderGlyphicon id =
              [buttonLink' (folderButtonParams id) [glyphicon_ c]]
          in
            Maybe.map doRenderFolderGlyphicon x.id

        folderButtonParams id =
          { class = ""
          , update =
            { onClick = Signal.message address (ToggleExpanded id)
            }
          }

        glyphicon =
          Maybe.withDefault [] (Maybe.map renderGlyphicon x.glyphicon)

        renderGlyphicon icon =
          [glyphicon_ icon]

        content =
          model.viewElement x.content

        contentWrap =
          Maybe.withDefault div_ (Maybe.map linkWrap x.href)

        linkWrap href =
          a' { href = href, class = "" }

        elemHtml =
          li' { class = "list-group-item" } (List.concat [indentElems, folderGlyphicon, glyphicon, [contentWrap [content]]])

        childrenHtml =
          if x.hasChildren && x.expanded
          then Maybe.withDefault [] (Maybe.map (List.concatMap (viewTreeItem (indent + 1))) x.children)
          else []
                      
      in
        (elemHtml :: childrenHtml)
            
  in
    ul' { class = "list-group" } (List.concatMap (viewTreeItem 0) model.elements)


update : Action a b -> TreeModel a b -> (TreeModel a b, Effects (Action a b))
update action model =
  case action of
    ToggleExpanded id ->
      let
        itemToggleExpanded (TreeItemModel element) =
          if element.id == Just id
          then
            let
              (eff, newChildren) =
                if element.expanded || not (Maybe.withDefault True (Maybe.map List.isEmpty element.children))
                then
                  (Nothing, element.children)
                else
                  (model.loadChildren id
                     |> Task.map (ChildrenLoaded id)
                     |> Effects.task
                     |> Just, Just [loadingPlaceholderTreeItemModel model.loadingPlaceholder])
            in
              (TreeItemModel { element |
                               expanded <- not element.expanded
                             , children <- newChildren
                             }, eff)
          else
            (TreeItemModel element, Nothing)

        elementsAndEffects =
          List.map itemToggleExpanded model.elements

        newModel =
          { model | elements <- List.map fst elementsAndEffects }

        newEffects =
          List.map snd elementsAndEffects
            |> Maybe.oneOf
            |> Maybe.withDefault Effects.none
                   
      in
        (newModel, newEffects)
    ChildrenLoaded id children ->
      let
        itemChildrenLoaded (TreeItemModel element) =
          case element.id of
            Just id ->
              TreeItemModel { element | children <- Just children }
            _ ->
              TreeItemModel element
        
        newModel =
          { model | elements <- List.map itemChildrenLoaded model.elements }
      in 
        (newModel, Effects.none)

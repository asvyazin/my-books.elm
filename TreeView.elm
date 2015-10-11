module TreeView (ViewContext, Action, TreeModel, TreeItemModel, TreeItemParams, item, folderItem, view, update) where

import Effects exposing (Effects, Never)
import EffectsExtensions as Effects
import Html exposing (Html)
import Html.Attributes as A
import Html.Events exposing (onClick)
import Html.Shorthand exposing (..)
import Maybe exposing (andThen)
import MaybeExtensions as Maybe
import MyAttributes
import Task exposing (Task)
import Tree


type Action b = Select b


type alias ViewContext a b =
  { actions : Signal.Address (Action b)
  , expand : Signal.Address b
  , viewContent : a -> Html
  }


type alias TreeModel a b = Tree.Forest (TreeItemModel a b)


type alias TreeItemModel a b =
  { params : TreeItemParams a b
  , expandable : Bool
  , expanded : Bool
  , selectable : Bool
  , selected : Bool 
  }


type alias TreeItemParams a b =
  { id : b
  , content : a
  , glyphicon : Maybe String
  , href : Maybe String
  }


item : TreeItemParams a b -> TreeItemModel a b
item p = initItem p False False


folderItem : TreeItemParams a b -> TreeItemModel a b
folderItem p = initItem p True True


initItem : TreeItemParams a b -> Bool -> Bool -> TreeItemModel a b
initItem p expandable selectable =
  { params = p
  , expandable = expandable 
  , expanded = False
  , selectable = selectable
  , selected = False
  }


glyphicon_ : String -> Html
glyphicon_ icon = span' { class = "glyphicon glyphicon-" ++ icon } []


view : ViewContext a b -> TreeModel a b -> Html
view context model =
  let
    -- viewTree : Int -> Tree.Tree (TreeItemMode a b) -> List Html
    viewTree indent (Tree.Tree item) =
      let
        x = item.data
        
        folderGlyphiconHtml =
          folderGlyphiconView context x
            |> Maybe.toList

        glyphiconHtml =
          Maybe.map glyphicon_ x.params.glyphicon
            |> Maybe.toList

        contentHtml =
          context.viewContent x.params.content

        contentWrap =
          Maybe.withDefault span_ (Maybe.map linkWrap x.params.href)

        linkWrap href =
          a' { href = href, class = "" }

        elemClass =
          A.classList [("list-group-item", True), ("selected", x.selected)]

        elemOnClick =
          if x.selectable
          then [onClick context.actions (Select x.params.id)]
          else []

        elemHtml =
          Html.li (List.append [ elemClass ] elemOnClick) (List.concat [indentView indent, folderGlyphiconHtml, glyphiconHtml, [contentWrap [contentHtml]]])

        childrenHtml =
          if x.expandable && x.expanded
          then
            item.children
              |> List.concatMap (viewTree (indent + 1))
          else []
                      
      in
        (elemHtml :: childrenHtml)
            
  in
    ul' { class = "list-group treeview" } (List.concatMap (viewTree 0) model)


indentView : Int -> List Html
indentView indent =
  List.repeat indent (span' { class = "indent" } [])


folderGlyphiconView : ViewContext a b -> TreeItemModel a b -> Maybe Html
folderGlyphiconView context item =
  if not item.expandable
  then Nothing
  else
    let
      glyphiconClass =
        if item.expanded
        then "minus"
        else "plus"

      -- renderFolderGlyphicon : b -> Html
      renderFolderGlyphicon id =
        Html.span [ class' "expand-folder", onClick context.expand id ] [ glyphicon_ glyphiconClass, Html.text " " ]
    in
      Just (renderFolderGlyphicon item.params.id)


update : Action b -> TreeModel a b -> TreeModel a b
update action model =
  case action of
    Select id ->
      let
        -- selectTree : Tree.Tree (TreeItemModel a b) -> Tree.Tree (TreeItemModel a b)
        selectItem item =
          if item.params.id == id
          then { item | selected <- True }
          else { item | selected <- False }
      in 
        List.map (Tree.map selectItem) model

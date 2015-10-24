module BootstrapModal where

import Debug
import Html exposing (Html)
import Html.Attributes as A
import ListExtensions as L
import MaybeExtensions as M
import MyAttributes as A


type BackdropType = Yes | No | Static


type alias ModalParams =
  { backdrop : Maybe BackdropType
  , keyboard : Maybe Bool
  , show : Maybe Bool
  , remote : Maybe String
  }


type alias FullModalOptions =
  { header : List Html
  , body : List Html
  , footer : List Html
  , params : ModalParams
  }


modalFull : FullModalOptions -> Html
modalFull options =
  let
    modalHeader = Html.div [A.class "modal-header"] options.header

    modalBody = Html.div [A.class "modal-body"] options.body

    modalFooter = Html.div [A.class "modal-footer"] options.footer
    
    content = Html.div [A.class "modal-content"] [modalHeader, modalBody, modalFooter]
    
    dialog = Html.div [A.class "modal-dialog"] [content]

    renderBackdrop backdrop =
      let
        backdropType =
          case backdrop of
            Yes -> "true"
            No -> "false"
            Static -> "static"
      in
        A.dataBackdrop backdropType

    renderKeyboard keyboard =
      let
        keyboardType = if keyboard then "true" else "false"
      in
        A.dataKeyboard keyboardType
       

    modalParams = M.filterJust
                  [ Maybe.map renderBackdrop options.params.backdrop
                  , Maybe.map renderKeyboard options.params.keyboard
                  , Maybe.map A.dataRemote options.params.remote
                  , Maybe.map A.dataShow (Debug.log "showModal" options.params.show)
                  ]
             
  in 
    Html.div ([A.class "modal fade", A.tabindex -1, A.role "dialog", A.ariaHidden True] ++ modalParams) [dialog]


type alias BasicModalOptions =
  { title : String
  , closable : Bool
  , body : List Html
  , footer : List Html 
  }


modalBasic : Bool -> BasicModalOptions -> Html
modalBasic show options =
  let
    modalHeader = Html.h4 [A.class "modal-title"] [Html.text options.title]

    closeButtonAttributes = [A.class "close", A.type' "button", A.dataDismiss "modal", A.ariaHidden True] 

    closeButton = Html.button closeButtonAttributes [Html.text "×"]
                  
  in 
    modalFull
    { header = L.elementsList [(True, modalHeader), (options.closable, closeButton)]
    , body = options.body
    , footer = options.footer
    , params =
      { remote = Nothing
      , show = Just show
      , keyboard = if options.closable then Nothing else Just False
      , backdrop = if options.closable then Nothing else Just Static
      }
    }

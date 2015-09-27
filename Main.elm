import Header

import Html exposing (..)
import Html.Attributes as A
import Html.Shorthand exposing (..)

main : Html
main = div' { class = "container" }
       [ node "link" [A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css", A.rel "stylesheet"] []
       , Header.view (Header.Model "Title" "User Name" ) ]

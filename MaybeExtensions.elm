module MaybeExtensions where

import ListExtensions as List
import Maybe exposing (Maybe)

toList : Maybe a -> List a
toList m = Maybe.withDefault [] (Maybe.map List.singleton m)

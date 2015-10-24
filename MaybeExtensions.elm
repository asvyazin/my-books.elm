module MaybeExtensions where

import ListExtensions as List
import Maybe exposing (Maybe)


toList : Maybe a -> List a
toList m = Maybe.withDefault [] (Maybe.map List.singleton m)


isJust : Maybe a -> Bool
isJust m =
  case m of
    Just _ -> True
    otherwise -> False


fromJust : Maybe a -> a
fromJust (Just x) = x


filterJust : List (Maybe a) -> List a
filterJust = List.filter isJust >> List.map fromJust

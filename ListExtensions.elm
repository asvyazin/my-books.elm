module ListExtensions where

singleton : a -> List a
singleton x = [x]


elementsList : List (Bool, a) -> List a
elementsList = List.filter fst >> List.map snd

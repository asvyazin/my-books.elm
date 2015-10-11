module Tree where

type Tree a =
  Tree
  { data : a
  , children : List (Tree a)
  }

            
type alias Forest a = List (Tree a)


map : (a -> b) -> Tree a -> Tree b
map f (Tree t) =
  let
    newData = f t.data
    newChildren = List.map (map f) t.children
  in
    Tree { data = newData, children = newChildren }

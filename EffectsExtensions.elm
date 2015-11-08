module EffectsExtensions where


import Effects exposing (Effects)


type alias Effects2 d a = (a, Effects d)


mapM : (a -> Effects2 c a) -> List a -> Effects2 c (List a)
mapM f l =
  let
    results = List.map f l
  in
    (List.map fst results, Effects.batch (List.map snd results))


bind : Effects2 d a -> (a -> Effects2 d b) -> Effects2 d b
bind (xa, eff1) f =
  let
    (xb, eff2) = f xa
  in
    (xb, Effects.batch [eff1, eff2])


noFx : model -> (model, Effects a)
noFx model =
    (model, Effects.none)

module Respo.EffectCollector

import Data.List
import Respo
import Respo.Effect

%default total

-- Collect all effects from a VNode tree with their paths
mutual
  public export
  covering
  collectEffects : List Int -> VNode -> List (List Int, List AnyEffect)
  collectEffects path (Text _) = []
  collectEffects path (Element el) = concatMap collectFromChild (indexedChildren el.children)
    where
      indexedChildren : List Child -> List (Int, Child)
      indexedChildren xs = go 0 xs
        where
          go : Int -> List Child -> List (Int, Child)
          go _ [] = []
          go idx (c :: cs) = (idx, c) :: go (idx + 1) cs

      collectFromChild : (Int, Child) -> List (List Int, List AnyEffect)
      collectFromChild (idx, MkChild _ node) = collectEffects (path ++ [idx]) node

  collectEffects path (Component comp) =
    if null comp.effects
      then collectEffects path comp.tree
      else (path, comp.effects) :: collectEffects path comp.tree

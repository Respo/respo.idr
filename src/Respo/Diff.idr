module Respo.Diff

import Data.List
import Respo

%default total

public export
data Patch
  = Replace (List Int) VNode
  | SetAttr (List Int) String (Maybe String)
  | SetStyle (List Int) String (Maybe String)
  | SetText (List Int) String
  | InsertChild (List Int) Int VNode
  | RemoveChild (List Int) Int

childNode : Child -> VNode
childNode (MkChild _ node) = node

childAttrs : RespoElement -> List (String, String)
childAttrs (MkRespoElement _ attrs _ _ _) = attrs

childStyles : RespoElement -> List (String, String)
childStyles (MkRespoElement _ _ styles _ _) = styles

lookupPair : String -> List (String, String) -> Maybe String
lookupPair _ [] = Nothing
lookupPair key ((k, v) :: rest) = if key == k then Just v else lookupPair key rest

snocPath : List Int -> Int -> List Int
snocPath path i = path ++ [i]

natToInt : Nat -> Int
natToInt Z = 0
natToInt (S k) = 1 + natToInt k

splitAtList : Int -> List a -> (List a, List a)
splitAtList n xs = splitAux n [] xs
  where
    splitAux : Int -> List a -> List a -> (List a, List a)
    splitAux 0 acc rest = (reverse acc, rest)
    splitAux _ acc [] = (reverse acc, [])
    splitAux k acc (y :: ys) = splitAux (k - 1) (y :: acc) ys

collectAttrDiff : List Int -> List (String, String) -> List (String, String) -> String -> List Patch -> List Patch
collectAttrDiff path oldAttrs newAttrs key patches =
  let old = lookupPair key oldAttrs
      new = lookupPair key newAttrs
  in if old == new then patches else SetAttr path key new :: patches

collectStyleDiff : List Int -> List (String, String) -> List (String, String) -> String -> List Patch -> List Patch
collectStyleDiff path oldStyles newStyles key patches =
  let old = lookupPair key oldStyles
      new = lookupPair key newStyles
  in if old == new then patches else SetStyle path key new :: patches

mutual
  covering
  diffAt : List Int -> VNode -> VNode -> List Patch
  diffAt path (Text a) (Text b) =
    if a == b then [] else [SetText path b]
  diffAt path (Element oldEl) (Element newEl) =
    if oldEl.tag /= newEl.tag then [Replace path (Element newEl)]
    else
      let attrPatches = diffAttrs path (childAttrs oldEl) (childAttrs newEl)
          stylePatches = diffStyles path (childStyles oldEl) (childStyles newEl)
          childPatches = diffChildren path oldEl.children newEl.children
      in attrPatches ++ stylePatches ++ childPatches
  diffAt path _ newNode = [Replace path newNode]

  diffAttrs : List Int -> List (String, String) -> List (String, String) -> List Patch
  diffAttrs path oldAttrs newAttrs =
    let keys = nub (map fst oldAttrs ++ map fst newAttrs)
    in foldr (collectAttrDiff path oldAttrs newAttrs) [] keys

  diffStyles : List Int -> List (String, String) -> List (String, String) -> List Patch
  diffStyles path oldStyles newStyles =
    let keys = nub (map fst oldStyles ++ map fst newStyles)
    in foldr (collectStyleDiff path oldStyles newStyles) [] keys

  covering
  diffChildren : List Int -> List Child -> List Child -> List Patch
  diffChildren path oldChildren newChildren =
    let sharedNat = min (length oldChildren) (length newChildren)
        shared = natToInt sharedNat
        (oldPrefix, oldTail) = splitAtList shared oldChildren
        (newPrefix, newTail) = splitAtList shared newChildren
        prefixPatches = diffChildPrefix path 0 oldPrefix newPrefix
        removePatches = removeTail path shared oldTail
        insertPatches = insertTail path shared newTail
    in prefixPatches ++ removePatches ++ insertPatches

  covering
  diffChildPrefix : List Int -> Int -> List Child -> List Child -> List Patch
  diffChildPrefix _ _ [] [] = []
  diffChildPrefix path idx (o :: os) (n :: ns) =
    diffAt (snocPath path idx) (childNode o) (childNode n) ++ diffChildPrefix path (idx + 1) os ns
  diffChildPrefix _ _ _ _ = []

  removeTail : List Int -> Int -> List Child -> List Patch
  removeTail _ _ [] = []
  removeTail path idx (_ :: rest) = RemoveChild path idx :: removeTail path idx rest

  insertTail : List Int -> Int -> List Child -> List Patch
  insertTail _ _ [] = []
  insertTail path idx (child :: rest) = InsertChild path idx (childNode child) :: insertTail path (idx + 1) rest

public export
covering
diff : VNode -> VNode -> List Patch
diff old new = diffAt [] old new

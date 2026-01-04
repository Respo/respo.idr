module Respo.Patcher

import Data.List
import Data.SortedSet
import Respo
import Respo.Diff
import Respo.Dom
import Respo.Effect

%default total

pathKey : List Int -> String
pathKey [] = ""
pathKey (x :: xs) = foldl (\acc, seg => acc ++ "." ++ seg) (show x) (map show xs)

covering
nodeHtml : VNode -> String
nodeHtml = render

runEffectsAt : String -> List Int -> EffectType -> List AnyEffect -> SortedSet Int -> IO ()
runEffectsAt rootId path effType effects skipIndexes =
  let elementId = rootId ++ "-" ++ pathKey path
  in runEffectsList elementId effType 0 effects skipIndexes
  where
    covering
    runEffectsList : String -> EffectType -> Int -> List AnyEffect -> SortedSet Int -> IO ()
    runEffectsList _ _ _ [] _ = pure ()
    runEffectsList elId ty idx (eff :: rest) skipped = do
      -- Only run effect if its index is not in the skipped set
      if contains idx skipped
        then pure ()  -- Skip this effect
        else runAnyEffect eff ty elId  -- Use runAnyEffect for polymorphic effects
      runEffectsList elId ty (idx + 1) rest skipped

public export
covering
applyPatch : String -> Patch -> IO ()
applyPatch rootId patch =
  let key = pathKey
  in case patch of
    Replace path node => replaceHtmlAt rootId (key path) (nodeHtml node)
    SetAttr path name value =>
      case value of
        Just v => setAttrAt rootId (key path) name v
        Nothing => removeAttrAt rootId (key path) name
    SetStyle path name value =>
      case value of
        Just v => setStyleAt rootId (key path) name v
        Nothing => removeStyleAt rootId (key path) name
    SetText path content => setTextAt rootId (key path) content
    InsertChild path idx node => insertHtmlAt rootId (key path) idx (nodeHtml node)
    RemoveChild path idx => removeChildAt rootId (key path) idx
    RunEffect path effType effects skipIndexes => runEffectsAt rootId path effType effects skipIndexes

public export covering
applyPatches : String -> List Patch -> IO ()
applyPatches _ [] = pure ()
applyPatches rootId (p :: rest) = do
  applyPatch rootId p
  applyPatches rootId rest

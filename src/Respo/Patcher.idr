module Respo.Patcher

import Data.List
import Respo
import Respo.Diff
import Respo.Dom

%default total

pathKey : List Int -> String
pathKey [] = ""
pathKey (x :: xs) = foldl (\acc, seg => acc ++ "." ++ seg) (show x) (map show xs)

covering
nodeHtml : VNode -> String
nodeHtml = render

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

public export covering
applyPatches : String -> List Patch -> IO ()
applyPatches _ [] = pure ()
applyPatches rootId (p :: rest) = do
  applyPatch rootId p
  applyPatches rootId rest

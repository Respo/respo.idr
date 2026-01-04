module Respo.StateTree

import Data.List
import Data.String

%default total

public export
record RespoStateTree where
  constructor MkRespoStateTree
  cursor : List String
  value : Maybe String
  snapshot : Maybe String
  branches : List (String, RespoStateTree)

public export
emptyStateTree : RespoStateTree
emptyStateTree = MkRespoStateTree [] Nothing Nothing []

lookupBranch : String -> List (String, RespoStateTree) -> Maybe RespoStateTree
lookupBranch _ [] = Nothing
lookupBranch key ((k, branch) :: rest) = if key == k then Just branch else lookupBranch key rest

setBranch : String -> RespoStateTree -> List (String, RespoStateTree) -> List (String, RespoStateTree)
setBranch key value [] = [(key, value)]
setBranch key value ((k, branch) :: rest) =
  if key == k then (key, value) :: rest else (k, branch) :: setBranch key value rest

public export
branchAt : RespoStateTree -> List String -> RespoStateTree
branchAt tree [] = tree
branchAt tree (key :: rest) =
  let next =
        case lookupBranch key tree.branches of
          Just child => child
          Nothing => MkRespoStateTree (tree.cursor ++ [key]) Nothing Nothing []
  in branchAt next rest

public export
path : RespoStateTree -> List String
path (MkRespoStateTree cursor _ _ _) = cursor

public export
pick : RespoStateTree -> String -> RespoStateTree
pick tree key = branchAt tree [key]

public export
record RespoUpdateState where
  constructor MkRespoUpdateState
  cursor : List String
  value : Maybe String
  snapshot : Maybe String

export partial
setIn : RespoStateTree -> RespoUpdateState -> RespoStateTree
setIn tree (MkRespoUpdateState [] newValue newSnapshot) =
  MkRespoStateTree tree.cursor newValue newSnapshot tree.branches
setIn tree (MkRespoUpdateState (key :: rest) newValue newSnapshot) =
  let next =
        case lookupBranch key tree.branches of
          Just child => child
          Nothing => MkRespoStateTree (tree.cursor ++ [key]) Nothing Nothing []
      updatedChild = setIn next (MkRespoUpdateState rest newValue newSnapshot)
      updatedBranches = setBranch key updatedChild tree.branches
  in MkRespoStateTree tree.cursor tree.value tree.snapshot updatedBranches

public export
interface RespoState state where
  defaultState : state
  encodeState : state -> Maybe String
  decodeState : Maybe String -> Either String state

public export
readState : RespoState state => RespoStateTree -> state
readState tree =
  case decodeState tree.value of
    Right value => value
    Left _ =>
      case decodeState tree.snapshot of
        Right value => value
        Left _ => defaultState

export partial
writeState : RespoState state => RespoStateTree -> List String -> state -> RespoStateTree
writeState tree cursor st =
  let encoded = encodeState st
  in setIn tree (MkRespoUpdateState cursor encoded encoded)

public export
readBranchState : RespoState state => RespoStateTree -> List String -> state
readBranchState tree cursor = readState (branchAt tree cursor)

public export
implementation RespoState Int where
  defaultState = 0
  encodeState n = Just (show n)
  decodeState Nothing = Right defaultState
  decodeState (Just raw) =
    case parseInteger raw of
      Just i => Right (fromInteger i)
      Nothing => Left "Failed to parse Int state"

public export
implementation RespoState String where
  defaultState = ""
  encodeState txt = Just txt
  decodeState Nothing = Right defaultState
  decodeState (Just raw) = Right raw

public export
implementation RespoState () where
  defaultState = ()
  encodeState _ = Nothing
  decodeState _ = Right ()

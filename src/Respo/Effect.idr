module Respo.Effect

import Respo.Dom

%default total

public export
data EffectType = Mounted | BeforeUpdate | Updated | BeforeUnmount

public export
Eq EffectType where
  Mounted == Mounted = True
  BeforeUpdate == BeforeUpdate = True
  Updated == Updated = True
  BeforeUnmount == BeforeUnmount = True
  _ == _ = False

public export
Show EffectType where
  show Mounted = "mounted"
  show BeforeUpdate = "beforeUpdate"
  show Updated = "updated"
  show BeforeUnmount = "beforeUnmount"

public export
record RespoEffectWithData a where
  constructor MkEffectWithData
  effectId : String
  effectData : a
  onMounted : Maybe (a -> String -> IO ())
  onBeforeUpdate : Maybe (a -> String -> IO ())
  onUpdated : Maybe (a -> String -> IO ())
  onBeforeUnmount : Maybe (a -> String -> IO ())

public export
Eq a => Eq (RespoEffectWithData a) where
  e1 == e2 = e1.effectId == e2.effectId

public export
Show (RespoEffectWithData a) where
  show eff = "Effect(" ++ eff.effectId ++ ")"

public export
data AnyEffect : Type where
  WrapEffect : Eq a => RespoEffectWithData a -> AnyEffect

public export
Eq AnyEffect where
  (WrapEffect e1) == (WrapEffect e2) = e1.effectId == e2.effectId

public export
Show AnyEffect where
  show (WrapEffect eff) = show eff

public export
getEffectId : AnyEffect -> String
getEffectId (WrapEffect eff) = eff.effectId

public export
RespoEffect : Type
RespoEffect = RespoEffectWithData ()

public export
MkEffect : String -> Maybe (String -> IO ()) -> Maybe (String -> IO ()) -> Maybe (String -> IO ()) -> Maybe (String -> IO ()) -> RespoEffect
MkEffect id onMount onBefore onUpdate onUnmount = MkEffectWithData id () (map (\f => \_, elId => f elId) onMount) (map (\f => \_, elId => f elId) onBefore) (map (\f => \_, elId => f elId) onUpdate) (map (\f => \_, elId => f elId) onUnmount)

public export
effectMounted : String -> (String -> IO ()) -> RespoEffect
effectMounted id callback = MkEffect id (Just callback) Nothing Nothing Nothing

public export
effectMountedWith : {a : _} -> Eq a => String -> a -> (a -> String -> IO ()) -> RespoEffectWithData a
effectMountedWith id d callback = MkEffectWithData id d (Just callback) Nothing Nothing Nothing

public export
effectUpdated : String -> (String -> IO ()) -> RespoEffect
effectUpdated id callback = MkEffect id Nothing Nothing (Just callback) Nothing

public export
effectUpdatedWith : {a : _} -> Eq a => String -> a -> (a -> String -> IO ()) -> RespoEffectWithData a
effectUpdatedWith id d callback = MkEffectWithData id d Nothing Nothing (Just callback) Nothing

public export
effectMountedAndUpdated : String -> (String -> IO ()) -> (String -> IO ()) -> RespoEffect
effectMountedAndUpdated id onMount onUpdate = MkEffect id (Just onMount) Nothing (Just onUpdate) Nothing

public export
effectMountedAndUpdatedWith : {a : _} -> Eq a => String -> a -> (a -> String -> IO ()) -> (a -> String -> IO ()) -> RespoEffectWithData a
effectMountedAndUpdatedWith id d onMount onUpdate = MkEffectWithData id d (Just onMount) Nothing (Just onUpdate) Nothing

public export
effectFull : String -> Maybe (String -> IO ()) -> Maybe (String -> IO ()) -> Maybe (String -> IO ()) -> Maybe (String -> IO ()) -> RespoEffect
effectFull = MkEffect

public export
effectFullWith : {a : _} -> Eq a => String -> a -> Maybe (a -> String -> IO ()) -> Maybe (a -> String -> IO ()) -> Maybe (a -> String -> IO ()) -> Maybe (a -> String -> IO ()) -> RespoEffectWithData a
effectFullWith = MkEffectWithData

public export covering
runEffect : RespoEffect -> EffectType -> String -> IO ()
runEffect eff Mounted elementId = case eff.onMounted of {Just callback => callback () elementId; Nothing => pure ()}
runEffect eff BeforeUpdate elementId = case eff.onBeforeUpdate of {Just callback => callback () elementId; Nothing => pure ()}
runEffect eff Updated elementId = case eff.onUpdated of {Just callback => callback () elementId; Nothing => pure ()}
runEffect eff BeforeUnmount elementId = case eff.onBeforeUnmount of {Just callback => callback () elementId; Nothing => pure ()}

public export covering
runEffectWithData : RespoEffectWithData a -> EffectType -> String -> IO ()
runEffectWithData eff Mounted elementId = case eff.onMounted of {Just callback => callback eff.effectData elementId; Nothing => pure ()}
runEffectWithData eff BeforeUpdate elementId = case eff.onBeforeUpdate of {Just callback => callback eff.effectData elementId; Nothing => pure ()}
runEffectWithData eff Updated elementId = case eff.onUpdated of {Just callback => callback eff.effectData elementId; Nothing => pure ()}
runEffectWithData eff BeforeUnmount elementId = case eff.onBeforeUnmount of {Just callback => callback eff.effectData elementId; Nothing => pure ()}

public export covering
runAnyEffect : AnyEffect -> EffectType -> String -> IO ()
runAnyEffect (WrapEffect eff) effType elementId = runEffectWithData eff effType elementId

public export
toAnyEffect : RespoEffect -> AnyEffect
toAnyEffect = WrapEffect

public export
wrapEffect : Eq a => RespoEffectWithData a -> AnyEffect
wrapEffect = WrapEffect

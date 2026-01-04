module Respo

import Data.List
import Respo.Effect
import Respo.Dom

%default total

mutual
  public export
  data VNode
    = Element RespoElement
    | Component RespoComponent
    | Text String

  public export
  record Child where
    constructor MkChild
    key : Maybe String
    node : VNode

  public export
  record RespoElement where
    constructor MkRespoElement
    tag : String
    attrs : List (String, String)
    style : List (String, String)
    listeners : List (String, String)
    children : List Child

  public export
  record RespoComponent where
    constructor MkRespoComponent
    name : String
    effects : List AnyEffect
    tree : VNode

indexChildren : Int -> List VNode -> List Child
indexChildren _ [] = []
indexChildren n (node :: rest) = MkChild (Just (show n)) node :: indexChildren (n + 1) rest

public export
el : String -> List (String, String) -> List VNode -> VNode
el tag attrs children =
  let keyed = indexChildren 0 children
  in Element (MkRespoElement tag attrs [] [] keyed)

public export
elKeyed : String -> List (String, String) -> List Child -> VNode
elKeyed tag attrs children = Element (MkRespoElement tag attrs [] [] children)

public export
text : String -> VNode
text = Text

public export
child : VNode -> Child
child node = MkChild Nothing node

public export
keyed : String -> VNode -> Child
keyed k node = MkChild (Just k) node

public export
withAttrs : VNode -> List (String, String) -> VNode
withAttrs (Element el) attrs =
  Element (MkRespoElement el.tag (el.attrs ++ attrs) el.style el.listeners el.children)
withAttrs node _ = node

public export
withStyle : VNode -> List (String, String) -> VNode
withStyle (Element el) extra =
  Element (MkRespoElement el.tag el.attrs (el.style ++ extra) el.listeners el.children)
withStyle node _ = node

public export
withListeners : VNode -> List (String, String) -> VNode
withListeners (Element el) listeners =
  Element (MkRespoElement el.tag el.attrs el.style (el.listeners ++ listeners) el.children)
withListeners node _ = node

public export
withChildren : VNode -> List Child -> VNode
withChildren (Element el) children =
  Element (MkRespoElement el.tag el.attrs el.style el.listeners (el.children ++ children))
withChildren node _ = node

renderAttrs : List (String, String) -> String
renderAttrs [] = ""
renderAttrs ((k, v) :: xs) = " " ++ k ++ "=\"" ++ v ++ "\"" ++ renderAttrs xs

renderStyles : List (String, String) -> String
renderStyles [] = ""
renderStyles ((k, v) :: xs) = k ++ ":" ++ v ++ ";" ++ renderStyles xs

mergeAttrs : List (String, String) -> List (String, String) -> List (String, String)
mergeAttrs attrs [] = attrs
mergeAttrs attrs styles = attrs ++ [("style", renderStyles styles)]

public export covering
render : VNode -> String
render (Text s) = s
render (Component comp) = render comp.tree
render (Element el) =
  let mergedAttrs = mergeAttrs el.attrs el.style
      openTag = "<" ++ el.tag ++ renderAttrs mergedAttrs ++ ">"
      closeTag = "</" ++ el.tag ++ ">"
      childrenHtml = renderChildren el.children
  in openTag ++ childrenHtml ++ closeTag
  where
    renderChildren : List Child -> String
    renderChildren [] = ""
    renderChildren (MkChild _ node :: rest) = render node ++ renderChildren rest

-- Create a component with effects (generic)
public export
comp : String -> List AnyEffect -> VNode -> VNode
comp name effects tree = Component (MkRespoComponent name effects tree)

-- Create a component with typed effects
public export
compWith : Eq a => String -> List (RespoEffectWithData a) -> VNode -> VNode
compWith name effects tree =
  Component (MkRespoComponent name (map wrapEffect effects) tree)

-- Create a component with simple effects
public export
compSimple : String -> List RespoEffect -> VNode -> VNode
compSimple name effects tree =
  Component (MkRespoComponent name (map toAnyEffect effects) tree)

-- Create a component without effects
public export
compEmpty : String -> VNode -> VNode
compEmpty name tree = Component (MkRespoComponent name [] tree)

-- Add an effect to a component
public export
withEffect : VNode -> AnyEffect -> VNode
withEffect (Component comp) eff =
  Component (MkRespoComponent comp.name (comp.effects ++ [eff]) comp.tree)
withEffect node _ = node

-- Add a typed effect to a component
public export
withEffectData : Eq a => VNode -> RespoEffectWithData a -> VNode
withEffectData node eff = withEffect node (wrapEffect eff)

-- Build DOM tree from VNode using Idris2 logic
export partial
buildDomTree : VNode -> String -> IO ()
buildDomTree tree elementId = do
  container <- primIO $ prim_getElementById elementId
  primIO $ prim_clearElement container
  node <- buildNode tree
  primIO $ prim_appendChild container node
  where
    buildNode : VNode -> IO AnyPtr
    buildNode (Text str) = primIO $ prim_createTextNode str
    buildNode (Component comp) = buildNode comp.tree
    buildNode (Element el) = do
      element <- primIO $ prim_createElement el.tag
      -- Set attributes
      traverse_ (\(k, v) => primIO $ prim_setAttribute element k v) el.attrs
      -- Set styles as a single style attribute
      let styleStr = concat $ intersperse ";" $ map (\(k, v) => k ++ ":" ++ v) el.style
      when (length el.style > 0) $
        primIO $ prim_setAttribute element "style" styleStr
      -- Build and append children
      for_ el.children $ \child => do
        childNode <- buildNode child.node
        primIO $ prim_appendChild element childNode
      pure element

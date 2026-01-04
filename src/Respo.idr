module Respo

import Data.List

%default total

mutual
  public export
  data VNode
    = Element RespoElement
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

module Respo.Dom

import System

%default total

%foreign "javascript:lambda:(id, content) => { const el = document.getElementById(id); if (el) el.innerHTML = content; }"
prim_setInnerHTML : String -> String -> PrimIO ()

export
setInnerHTML : String -> String -> IO ()
setInnerHTML id content = primIO $ prim_setInnerHTML id content

%foreign "javascript:lambda:(msg) => console.log(msg)"
prim_log : String -> PrimIO ()

export
log : String -> IO ()
log msg = primIO $ prim_log msg

locateSnippet : String
locateSnippet = "const container = document.getElementById(rootId);\n"
	++ "if (!container) { return; }\n"
	++ "const locateNode = () => {\n"
	++ "  const first = container.firstChild;\n"
	++ "  if (!first) { return null; }\n"
	++ "  if (!path || path.length === 0) { return first; }\n"
	++ "  const parts = path.split('.').filter(s => s.length > 0);\n"
	++ "  let node = first;\n"
	++ "  for (const part of parts) {\n"
	++ "    const idx = parseInt(part, 10);\n"
	++ "    if (!node.childNodes || idx < 0 || idx >= node.childNodes.length) { return null; }\n"
	++ "    node = node.childNodes[idx];\n"
	++ "  }\n"
	++ "  return node;\n"
	++ "};\n"
	++ "const target = locateNode();\n"
	++ "if (!target) { return; }\n"

createNodeSnippet : String
createNodeSnippet = "const template = document.createElement('template');\n"
	++ "template.innerHTML = html;\n"
	++ "const fresh = template.content.firstChild;\n"
	++ "if (!fresh) { return; }\n"

replaceHtmlCode : String
replaceHtmlCode = "javascript:lambda:(rootId, path, html) => {\n"
	++ locateSnippet ++ createNodeSnippet ++ "target.replaceWith(fresh);\n}"

setTextCode : String
setTextCode = "javascript:lambda:(rootId, path, value) => {\n"
	++ locateSnippet ++ "target.textContent = value;\n}"

setAttrCode : String
setAttrCode = "javascript:lambda:(rootId, path, key, value) => {\n"
	++ locateSnippet ++ "if (target.nodeType !== 1) { return; }\n"
	++ "target.setAttribute(key, value);\n}"

removeAttrCode : String
removeAttrCode = "javascript:lambda:(rootId, path, key) => {\n"
	++ locateSnippet ++ "if (target.nodeType !== 1) { return; }\n"
	++ "target.removeAttribute(key);\n}"

setStyleCode : String
setStyleCode = "javascript:lambda:(rootId, path, key, value) => {\n"
	++ locateSnippet ++ "if (target.nodeType !== 1 || !target.style) { return; }\n"
	++ "target.style.setProperty(key, value);\n}"

removeStyleCode : String
removeStyleCode = "javascript:lambda:(rootId, path, key) => {\n"
	++ locateSnippet ++ "if (target.nodeType !== 1 || !target.style) { return; }\n"
	++ "target.style.removeProperty(key);\n}"

insertHtmlCode : String
insertHtmlCode = "javascript:lambda:(rootId, path, index, html) => {\n"
	++ locateSnippet ++ createNodeSnippet
	++ "const ref = (index >= 0 && index < target.childNodes.length) ? target.childNodes[index] : null;\n"
	++ "target.insertBefore(fresh, ref);\n}"

removeChildCode : String
removeChildCode = "javascript:lambda:(rootId, path, index) => {\n"
	++ locateSnippet ++ "const child = target.childNodes && target.childNodes[index];\n"
	++ "if (child) { target.removeChild(child); }\n}"

%foreign replaceHtmlCode
prim_replaceHtmlAt : String -> String -> String -> PrimIO ()

export
replaceHtmlAt : String -> String -> String -> IO ()
replaceHtmlAt rootId path html = primIO $ prim_replaceHtmlAt rootId path html

%foreign setTextCode
prim_setTextAt : String -> String -> String -> PrimIO ()

export
setTextAt : String -> String -> String -> IO ()
setTextAt rootId path value = primIO $ prim_setTextAt rootId path value

%foreign setAttrCode
prim_setAttrAt : String -> String -> String -> String -> PrimIO ()

export
setAttrAt : String -> String -> String -> String -> IO ()
setAttrAt rootId path key value = primIO $ prim_setAttrAt rootId path key value

%foreign removeAttrCode
prim_removeAttrAt : String -> String -> String -> PrimIO ()

export
removeAttrAt : String -> String -> String -> IO ()
removeAttrAt rootId path key = primIO $ prim_removeAttrAt rootId path key

%foreign setStyleCode
prim_setStyleAt : String -> String -> String -> String -> PrimIO ()

export
setStyleAt : String -> String -> String -> String -> IO ()
setStyleAt rootId path key value = primIO $ prim_setStyleAt rootId path key value

%foreign removeStyleCode
prim_removeStyleAt : String -> String -> String -> PrimIO ()

export
removeStyleAt : String -> String -> String -> IO ()
removeStyleAt rootId path key = primIO $ prim_removeStyleAt rootId path key

%foreign insertHtmlCode
prim_insertHtmlAt : String -> String -> Int -> String -> PrimIO ()

export
insertHtmlAt : String -> String -> Int -> String -> IO ()
insertHtmlAt rootId path index html = primIO $ prim_insertHtmlAt rootId path index html

%foreign removeChildCode
prim_removeChildAt : String -> String -> Int -> PrimIO ()

export
removeChildAt : String -> String -> Int -> IO ()
removeChildAt rootId path index = primIO $ prim_removeChildAt rootId path index

%foreign "javascript:lambda:() => (window.respoTakeAction ? window.respoTakeAction() : '')"
prim_nextAction : PrimIO String

export
nextAction : IO String
nextAction = primIO prim_nextAction

%foreign """
javascript:lambda:(resume, world) => {
	const invoke = () => resume(world);
	if (typeof window !== 'undefined' && typeof window.respoRegisterActionWaiter === 'function') {
		window.respoRegisterActionWaiter(invoke);
		return;
	}
	if (typeof window !== 'undefined') {
		if (!window.respoActionWaiters) {
			window.respoActionWaiters = [];
		}
		if (window.respoActionQueue && window.respoActionQueue.length > 0) {
			if (typeof queueMicrotask === 'function') {
				queueMicrotask(invoke);
			} else {
				setTimeout(invoke, 0);
			}
		} else {
			window.respoActionWaiters.push(invoke);
		}
		return;
	}
	setTimeout(invoke, 0);
}
"""
prim_waitForNextAction : IO () -> PrimIO ()

export
waitForNextAction : IO () -> IO ()
waitForNextAction resume = primIO $ prim_waitForNextAction resume

-- Build real DOM tree from VNode
buildDomTreeCode : String
buildDomTreeCode = """
javascript:lambda:(tree, elementId) => {
  const buildNode = (vnode) => {
    // Handle null/undefined
    if (!vnode) {
      return document.createTextNode('');
    }

    // Text node (constructor index 2)
    if (vnode.h === 2) {
      return document.createTextNode(vnode.a1 || '');
    }

    // Component node (constructor index 1) - render its tree
    if (vnode.h === 1) {
      const comp = vnode.a1;
      if (comp && comp.a3) {
        return buildNode(comp.a3);
      }
      return document.createTextNode('');
    }

    // Element node (constructor index 0)
    if (vnode.h === 0) {
      const el = vnode.a1;
      if (!el) {
        return document.createTextNode('');
      }

      const tag = el.a1;
      if (!tag) {
        return document.createTextNode('');
      }

      const element = document.createElement(tag);

      // Helper to iterate Idris2 List
      // List Cons has a1 (head) and a2 (tail)
      // Empty list is when a1 is undefined or a2 is missing
      const listToArray = (list) => {
        const result = [];
        let current = list;
        while (current && (current.h === 0 || (current.a1 && current.a2 !== undefined))) {
          result.push(current.a1);
          current = current.a2;
        }
        return result;
      };

      // Set attributes
      const attrs = listToArray(el.a2 || {});
      for (const pair of attrs) {
        if (pair && pair.a1 && pair.a2) {
          element.setAttribute(pair.a1, pair.a2);
        }
      }

      // Set styles
      const styles = listToArray(el.a3 || {});
      const styleStrings = [];
      for (const pair of styles) {
        if (pair && pair.a1 && pair.a2) {
          styleStrings.push(pair.a1 + ':' + pair.a2);
        }
      }
      if (styleStrings.length > 0) {
        element.setAttribute('style', styleStrings.join(';'));
      }

      // Build and append children
      const children = listToArray(el.a5 || {});
      for (const child of children) {
        if (child && child.a2) {
          element.appendChild(buildNode(child.a2));
        }
      }

      return element;
    }

    // Fallback
    return document.createTextNode('');
  };

  const container = document.getElementById(elementId);
  if (!container) {
    console.error('Container not found:', elementId);
    return;
  }

  const node = buildNode(tree);
  container.innerHTML = '';
  container.appendChild(node);
}
"""

%foreign buildDomTreeCode
prim_buildDomTree : (tree : AnyPtr) -> String -> PrimIO ()

export
buildDomTree : a -> String -> IO ()
buildDomTree tree elementId = primIO $ prim_buildDomTree (believe_me tree) elementId

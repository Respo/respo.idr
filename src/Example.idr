module Example

import Data.List
import Data.String
import Respo
import Respo.Dom
import Respo.Diff
import Respo.Patcher
import Respo.Style
import Respo.StateTree
import Respo.Effect
import Respo.EffectCollector
import System

%default total

public export
record Todo where
  constructor MkTodo
  text : String
  done : Bool

public export
Eq Todo where
  (MkTodo t1 d1) == (MkTodo t2 d2) = t1 == t2 && d1 == d2

public export
data UserAction
  = Increment
  | AddTodo
  | ToggleTodo Int
  | ClearDone

public export
record Store where
  constructor MkStore
  states : RespoStateTree
  count : Int
  history : List Int
  todos : List Todo
  nextTodoId : Int

keepRecent : Int -> List Int -> List Int
keepRecent _ [] = []
keepRecent 0 _ = []
keepRecent n (x :: xs) = x :: keepRecent (n - 1) xs

mapIndexed : (Int -> a -> b) -> List a -> List b
mapIndexed fn = go 0
  where
    go : Int -> List a -> List b
    go _ [] = []
    go idx (x :: rest) = fn idx x :: go (idx + 1) rest

index : Int -> List a -> Maybe a
index _ [] = Nothing
index 0 (x :: _) = Just x
index n (_ :: rest) = index (n - 1) rest

natToInt : Nat -> Int
natToInt Z = 0
natToInt (S k) = 1 + natToInt k

initialTodos : List Todo
initialTodos =
  [ MkTodo "Sketch light UI" False
  , MkTodo "Add todo demo" False
  ]

initialStore : Store
initialStore =
  let zero : Int
      zero = 0
      states0 = writeState emptyStateTree ["counter"] zero
      states1 = writeState states0 ["status"] "Waiting for input"
      nextId = natToInt (length initialTodos)
  in MkStore states1 0 [] initialTodos nextId

incrementCounter : Store -> Store
incrementCounter (MkStore states count history todos nextId) =
  let nextCount = count + 1
      nextHistory = keepRecent 5 (nextCount :: history)
      states0 = writeState states ["counter"] nextCount
      states1 = writeState states0 ["status"] ("Count updated to " ++ show nextCount)
  in MkStore states1 nextCount nextHistory todos nextId

addTodoEntry : Store -> Store
addTodoEntry (MkStore states count history todos nextId) =
  let label = "Todo #" ++ show (nextId + 1)
      todo = MkTodo label False
      states1 = writeState states ["status"] ("Added " ++ label)
  in MkStore states1 count history (todo :: todos) (nextId + 1)

toggleTodoAt : Int -> Store -> Store
toggleTodoAt idx (MkStore states count history todos nextId) =
  let updated = mapIndexed (\i, todo => if i == idx then MkTodo todo.text (not todo.done) else todo) todos
      statusMsg =
        case index idx todos of
          Just todo =>
            if todo.done then "Marked " ++ todo.text ++ " as active"
            else "Marked " ++ todo.text ++ " as done"
          Nothing => "Toggled todo"
      states1 = writeState states ["status"] statusMsg
  in MkStore states1 count history updated nextId

clearDoneTodos : Store -> Store
clearDoneTodos (MkStore states count history todos nextId) =
  let remaining = filter (\todo => not todo.done) todos
      removed = natToInt (length todos) - natToInt (length remaining)
      statusMsg =
        if removed == 0 then "No completed todos to clear" else "Cleared " ++ show removed ++ " completed"
      states1 = writeState states ["status"] statusMsg
  in MkStore states1 count history remaining nextId

applyAction : Store -> UserAction -> Store
applyAction store Increment = incrementCounter store
applyAction store AddTodo = addTodoEntry store
applyAction store (ToggleTodo idx) = toggleTodoAt idx store
applyAction store ClearDone = clearDoneTodos store

splitActionString : String -> (String, String)
splitActionString str =
  let (leftPart, rightPart) = go [] (unpack str)
  in (pack leftPart, pack rightPart)
  where
    go : List Char -> List Char -> (List Char, List Char)
    go acc [] = (reverse acc, [])
    go acc (':' :: rest) = (reverse acc, rest)
    go acc (ch :: rest) = go (ch :: acc) rest

parseActionString : String -> Maybe UserAction
parseActionString raw =
  let (name, payload) = splitActionString raw
  in case name of
    "" => Nothing
    "increment" => Just Increment
    "add-todo" => Just AddTodo
    "clear-done" => Just ClearDone
    "toggle" =>
      case parseInteger payload of
        Just n => Just (ToggleTodo (fromInteger n))
        Nothing => Nothing
    _ => Nothing

covering
drainActions : IO (List UserAction)
drainActions = do
  raw <- nextAction
  if raw == "" then pure [] else case parseActionString raw of
      Nothing => drainActions
      Just action => do
        rest <- drainActions
        pure (action :: rest)

actionLabel : UserAction -> String
actionLabel Increment = "increment"
actionLabel AddTodo = "add-todo"
actionLabel (ToggleTodo idx) = "toggle:" ++ show idx
actionLabel ClearDone = "clear-done"

-- Effect example: log when counter is mounted and updated
counterEffect : RespoEffect
counterEffect = effectMountedAndUpdated "counter-effect"
  (\elementId => log ("Counter mounted at: " ++ elementId))
  (\elementId => log ("Counter updated at: " ++ elementId))

-- Example with structured data: effect that carries counter value
counterEffectWithCount : Int -> RespoEffectWithData Int
counterEffectWithCount count =
  effectMountedAndUpdatedWith
    ("counter-with-data-" ++ show count)  -- ID changes with count
    count                                   -- Data carried with effect
    (\c, elementId => log ("Counter mounted with value: " ++ show c ++ " at: " ++ elementId))
    (\c, elementId => log ("Counter updated to: " ++ show c ++ " at: " ++ elementId))

-- Example with structured Todo data (not used yet, for demonstration)
todoEffect : Todo -> RespoEffectWithData Todo
todoEffect todo =
  effectUpdatedWith
    ("todo-" ++ todo.text ++ "-" ++ show todo.done)
    todo
    (\t, elementId =>
      log ("Todo effect - text: " ++ t.text ++ ", done: " ++ show t.done ++ ", at: " ++ elementId))

renderCounterCard : Store -> VNode
renderCounterCard store =
  let status = readBranchState store.states ["status"]
      header = withStyle (el "p" [] [ text "Counter" ]) (finish (fontSize 14 (color "#6b7280" style)))
      value = withStyle (el "h1" [] [ text (show store.count) ]) (finish (fontSize 40 (fontWeight "700" style)))
      incrementBtn = withStyle (el "button" [("data-action", "increment")] [ text "+1" ]) uiButton
      toolbar = withStyle (el "div" [] [incrementBtn]) (finish (gap 12 (display "flex" style)))
      statusLine = withStyle (el "p" [] [ text status ]) (finish (color "#4b5563" style))
      content = withStyle (el "div" [] [header, value, toolbar, statusLine]) uiPanel
      -- Demo: Mix simple effect with structured data effect
      -- The simple effect logs when counter is mounted/updated
      -- The data effect carries the count value and logs it
      simpleEff = toAnyEffect counterEffect
      dataEff = wrapEffect (counterEffectWithCount store.count)
  in comp "counter-card" [simpleEff, dataEff] content

renderTodoItem : Int -> Todo -> VNode
renderTodoItem idx todo =
  let labelText = if todo.done then todo.text ++ " (done)" else todo.text
      toggleLabel = if todo.done then "Undo" else "Complete"
      toggleBtn =
        withStyle
          (el "button" [("data-action", "toggle"), ("data-payload", show idx)] [ text toggleLabel ])
          uiButtonSecondary
      row =
        withStyle
          (el "div" [] [ text labelText, toggleBtn ])
          ( finish
              ( gap 12
              ( display "flex"
              ( alignItems "center"
              ( custom "justify-content" "space-between" style)))))
  in el "li" [] [row]

renderTodoList : Store -> VNode
renderTodoList store =
  let heading = withStyle (el "h2" [] [ text "Todos" ]) (finish (fontSize 20 (fontWeight "600" style)))
      addBtn = withStyle (el "button" [("data-action", "add-todo")] [ text "Add quick todo" ]) uiButton
      clearBtn = withStyle (el "button" [("data-action", "clear-done")] [ text "Clear completed" ]) uiButtonSecondary
      controls =
        withStyle (el "div" [] [addBtn, clearBtn])
          (finish (gap 12 (display "flex" (flexDirection "row" style))))
      items =
        case store.todos of
          [] => [el "li" [] [ text "No items yet" ]]
          _ => mapIndexed renderTodoItem store.todos
      listView = withStyle (el "ul" [] items) (finish (gap 12 (display "flex" (flexDirection "column" style))))
  in withStyle (el "div" [] [heading, controls, listView]) uiPanel

renderHistoryCard : List Int -> VNode
renderHistoryCard entries =
  let heading = withStyle (el "h2" [] [ text "Recent clicks" ]) (finish (fontSize 20 (fontWeight "600" style)))
      items =
        case entries of
          [] => [el "li" [] [ text "No clicks tracked" ]]
          _ => map (\tick => el "li" [] [ text ("#" ++ show tick) ]) entries
      listView = withStyle (el "ul" [] items) (finish (gap 6 (display "flex" (flexDirection "column" style))))
  in withStyle (el "div" [] [heading, listView]) uiPanel

public export
renderApp : Store -> VNode
renderApp store =
  let cards =
        [ renderCounterCard store
        , renderTodoList store
        , renderHistoryCard store.history
        ]
      layout =
        withStyle (el "div" [] cards)
          ( finish
              ( custom "display" "grid"
              ( custom "grid-template-columns" "repeat(auto-fit, minmax(280px, 1fr))"
              ( gap 24 style))))
  in withStyle layout uiGlobal

renderStandaloneCounter : Int -> VNode
renderStandaloneCounter n =
  withStyle
    ( el "div" []
      [ withStyle (el "p" [] [ text "Counter" ]) (finish (fontSize 14 (color "#6b7280" style)))
      , withStyle (el "h1" [] [ text (show n) ]) (finish (fontSize 40 (fontWeight "700" style)))
      , withStyle (el "button" [] [ text "+" ]) uiButton
      ])
    uiPanel

public export covering
renderCounterHTML : Int -> String
renderCounterHTML n = render (renderStandaloneCounter n)

public export covering
renderAppHTML : Store -> String
renderAppHTML store = render (renderApp store)

processActions : Store -> List UserAction -> Store
processActions store [] = store
processActions store (action :: rest) = processActions (applyAction store action) rest

covering
loop : String -> VNode -> Store -> IO ()
loop rootId prev store = do
  actions <- drainActions
  case actions of
    [] => waitForNextAction (loop rootId prev store)
    _ => do
      let nextStore = processActions store actions
          nextVNode = renderApp nextStore
      applyPatches rootId (diff prev nextVNode)
      log ("Actions: " ++ show (map actionLabel actions))
      loop rootId nextVNode nextStore

pathToString : List Int -> String
pathToString [] = ""
pathToString (x :: xs) = foldl (\acc, seg => acc ++ "." ++ seg) (show x) (map show xs)

covering
runEffectList : String -> List AnyEffect -> IO ()
runEffectList _ [] = pure ()
runEffectList elId (eff :: rest) = do
  runAnyEffect eff Mounted elId  -- Use runAnyEffect for polymorphic effects
  runEffectList elId rest

covering
runAllMountedEffects : String -> List (List Int, List AnyEffect) -> IO ()
runAllMountedEffects _ [] = pure ()
runAllMountedEffects rootId ((path, effects) :: rest) = do
  let elementId = rootId ++ "-" ++ pathToString path
  runEffectList elementId effects
  runAllMountedEffects rootId rest

covering
runMountedEffects : String -> VNode -> IO ()
runMountedEffects rootId vnode = do
  let effectList = collectEffects [] vnode
  runAllMountedEffects rootId effectList

covering
main : IO ()
main = do
  let rootId = "app"
      store0 = initialStore
      vnode0 = renderApp store0
  buildDomTree vnode0 rootId
  log "Mounted Respo demo"
  runMountedEffects rootId vnode0
  loop rootId vnode0 store0

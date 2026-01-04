# Idris2 Respo

Idris2 implementation of Respo, a virtual DOM library with effects system. This is a port of the core ideas from [rs-respo](https://github.com/Respo/rs.respo).

## Features

- 🌳 Virtual DOM with efficient diffing
- 🎯 Component-based architecture
- ⚡ Effect system (mount/unmount/update hooks)
- 🔄 Parameterized effects with change detection
- 🎨 Type-safe rendering

## Prerequisites

- [Idris2](https://github.com/idris-lang/Idris2) v0.8.0 or later
- Python 3 (for local development server)

## Quick Start

### Build

```bash
idris2 --codegen javascript --build respo.ipkg
```

### Development

```bash
idris2 --codegen javascript --build respo.ipkg
python3 -m http.server 8080
```

Then open http://localhost:8080 in your browser.

## Project Structure

```
src/
├── Respo.idr              # Core VNode and component definitions
├── Respo/
│   ├── Dom.idr            # DOM FFI primitives
│   ├── Diff.idr           # Virtual DOM diffing
│   ├── Patcher.idr        # DOM patching operations
│   ├── Effect.idr         # Effect system
│   ├── EffectCollector.idr # Effect collection from VNode tree
│   └── StateTree.idr      # State tree management
└── Example.idr            # Demo application
```

## Building and Deploying

### Build

```bash
# Build
idris2 --codegen javascript --build respo.ipkg

# Create distribution
mkdir -p dist
cp index.html dist/
cp build/exec/respo dist/respo.js
```

### Deploy with rsync

```bash
rsync -avzr --progress dist/* rsync-user@tiye.me:/web-assets/repo/Respo/idr-respo/
```

## GitHub Actions

Automated build and deployment is configured via GitHub Actions:

- **Build and Deploy** (`.github/workflows/build-and-deploy.yaml`): Runs on push to main branch
  - Uses Docker container with Idris2 pre-installed
  - Builds the project
  - Deploys to remote server via rsync

## Development

### Effect System

Effects provide lifecycle hooks for components:

```idris
-- Simple effect
counterEffect : RespoEffect
counterEffect = effectMounted "counter-effect" $ \coord => do
  log $ "Counter mounted at: " ++ coord

-- Parameterized effect with data
counterEffectWithCount : Int -> RespoEffectWithData Int
counterEffectWithCount n = effectMountedWith "counter-with-data" n $ \coord, count => do
  log $ "Counter mounted with value: " ++ show count ++ " at: " ++ coord
```

### Component Pattern

```idris
comp "my-component"
  [counterEffect]          -- Effects
  [todoEffect myTodo]      -- Parameterized effects
  (div [] [               -- Virtual DOM tree
    h1 [] [text "Hello"],
    button [("data-action", "click")] [text "Click me"]
  ])
```

## License

MIT

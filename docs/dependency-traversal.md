# Dependency Traversal

Atlas provides bidirectional recursive dependency traversal: forward (what does X need?) and reverse (what breaks if X changes?).

## Forward: "What do I need?"

```
:fn/a -> :fn/b -> :fn/d
      -> :fn/c -> :fn/d (already seen)
```

- `recursive-dependencies-of` — BFS tree with depth, via, already-seen markers
- `recursive-dependencies-summary` — flat lists: all entity deps + all data keys needed

Context/input keys are classified: execution-functions and structure-components are deps, everything else is data input.

## Reverse: "What breaks if I change this?"

```
:fn/d <- :fn/b <- :fn/a
      <- :fn/c <- :fn/a (already seen)
```

- `recursive-dependents-of` — reverse BFS tree (same structure as forward)
- `recursive-dependents-summary` — blast radius: affected count, grouped by entity type

## Keybindings

| Direction | Tree view | Summary |
|-----------|-----------|---------|
| Forward (deps) | `t` / `C-c a t` | `w` / `C-c a w` |
| Reverse (dependents) | `R` / `C-c a R` | `b` / `C-c a b` |
| Direct deps only | `d` / `C-c a d` | |
| Direct dependents only | `r` / `C-c a r` | |

## API

```clojure
(require '[atlas.ide :as ide])

;; Forward
(ide/recursive-dependencies-of :fn/my-function)
;; => [{:dep/dev-id :fn/b :dep/type :atlas/execution-function :dep/depth 1 :dep/via :fn/a ...} ...]

(ide/recursive-dependencies-summary :fn/my-function)
;; => {:summary/root :fn/a :summary/deps [...] :summary/data-keys [...]}

;; Reverse
(ide/recursive-dependents-of :fn/my-function)
;; => [{:dep/dev-id :fn/x :dep/type :atlas/execution-function :dep/depth 1 :dep/via :fn/a ...} ...]

(ide/recursive-dependents-summary :fn/my-function)
;; => {:summary/root :fn/a :summary/affected [...] :summary/affected-count 3 :summary/by-type {...}}
```

## How it works

Both directions use BFS with a visited set for cycle safety. Shared nodes appear inline with `:dep/already-seen? true` (shown as `↑` in the tree view) but aren't recursed into again.

Dependencies are resolved via `effective-dependencies-for`, which combines:
1. Explicit deps (`:execution-function/deps`, `:structure-component/deps`)
2. Dataflow-derived deps (producer/consumer matching on context/response keys)

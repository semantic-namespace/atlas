# Atlas Keybindings

## Result Buffers (atlas-mode)

Single-key access. Commands use entity/data-key at point, fall back to completing-read.

| Key | Action |
|-----|--------|
| `q` | Quit |
| `g` | Refresh |
| `?` | Open transient menu |
| `i` | Entity info |
| `f` | Drill into props |
| `a` | Find by aspect |
| `d` | Dependencies |
| `t` | Transitive deps (recursive) |
| `w` | "What do I need?" summary |
| `r` | Dependents (reverse) |
| `p` | Producers of data-key |
| `u` | Consumers of data-key |
| `c` | Check invariants |
| `x` | Execution order |
| `h` | History menu |

## Clojure Buffers (atlas-clj-mode)

Minor mode with `C-c a` prefix. Enable with:

```elisp
(add-hook 'clojure-mode-hook #'atlas-clj-mode-enable)
```

| Key | Action |
|-----|--------|
| `C-c a a` | Open transient menu |
| `C-c a i` | Entity info at point |
| `C-c a t` | Transitive deps at point |
| `C-c a w` | "What do I need?" at point |
| `C-c a d` | Dependencies at point |
| `C-c a r` | Dependents at point |
| `C-c a f` | Drill into props at point |
| `C-c a l` | Toggle lens mode |
| `C-c a c` | Check invariants |

## Go to Definition (M-.)

`M-.` on a registered Atlas keyword jumps to the `register!` call site.
`M-,` returns to where you were. Works in both clojure and atlas buffers.

- Uses REPL to confirm the keyword is a registered entity (no false positives)
- Uses ripgrep/grep to find the source location (handles any alias: `registry/`, `cid/`, `r/`, etc.)
- Falls through to CIDER/LSP for non-Atlas keywords

## Global

| Key | Action |
|-----|--------|
| `M-F` | Open Atlas transient menu |

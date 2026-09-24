# atlas-emacs — design notes

## The model

```
 Claude Code (server terminal UI)            human's other terminal
 ─────────────────────────────────           ──────────────────────
 atlas-llm-daemon.sh ensure ──┐              $ em   (= … attach)
                              ▼                 │ emacsclient -t
                  emacs --daemon=<abs socket>  ◄┘ (client frame, tab per layout)
                    │ CIDER (cider-connect)
                    ▼
                  project nREPL  ◄── clj-nrepl-eval (LLM reads ground truth)
```

- **The LLM owns setup.** It picks the REPL by project directory, starts the
  daemon in that directory, connects CIDER. The human's only step is `em`.
- **One daemon per project directory**, on socket `atlas-<basename>-<hash of
  the full path>`: two checkouts with the same folder name (worktrees) never
  share a daemon. **Each daemon talks to exactly one REPL**; connecting to a
  different port closes the old CIDER connection (the REPL itself keeps
  running), so views can't query a stale REPL.
- **Two readable sides.** The REPL is ground truth; `llm-screen` returns what
  the human actually sees. The LLM verifies one against the other instead of
  asking the human to describe their screen.
- **Intent, not selection.** The LLM derives the views that fit and labels them
  by what the human anchors on; the human picks.

## Code map

| Piece | Where |
|---|---|
| Daemon lifecycle (`ensure`/`status`/`stop`) | `emacs/atlas-llm-daemon.sh` |
| Layouts + frame/tab targeting + `llm-*` helpers | `emacs/atlas-layout.el` |
| Pane contents | `emacs/atlas-browse.el` (calls `atlas.ide/*` over CIDER) |
| Stylesheet: faces (light/dark) + building blocks | `emacs/atlas-theme.el` |
| Registration lookup for the source pane | `atlas.tooling.lsp-helpers/find-definition` |
| Attach | `atlas-llm-daemon.sh attach`; each person aliases it (`em`) |

## Decisions and the gotchas behind them

- **Absolute socket paths.** Snap-confined `emacsclient` does not find
  name-only sockets.
- **Emacs >= 27 is picked explicitly.** A distro `emacs` on PATH can be much
  older (no tab-bar) than the Emacs the person actually uses (e.g. a snap).
- **Layouts target the latest attached client frame**
  (`server-after-make-frame-hook`). `--eval` runs in the daemon's invisible
  initial frame, and a suspended older client can hold a second frame on the
  same tty.
- **One tab per layout** instead of `delete-other-windows` on the human's only
  view: previous views stay reachable, and re-running reuses the tab.
- **Narrow frames stack panes** (`atlas-layout-narrow-width`, 140 cols).
- **`find-definition` ranks text-search hits by aspect overlap.** The registry
  stores no source location, and test fixtures reuse dev-ids with other aspects.
- **Ontology preflight in `ensure`.** `deps-for` reads dep keys from registered
  ontologies; example registries don't load them, and then every dependency
  view is silently empty.
- **Styling lives in `atlas-theme.el`, not in views.** Views compose building
  blocks (`atlas-theme-title`, `-section`, `-row`, `-badge`, `-entity-list`,
  `-mark`, `-footer`); faces carry light and dark variants and Emacs picks per
  frame. Terminal-safe only: foreground, background, weight, slant.
- **Preferences travel with the attaching terminal.** `emacsclient -t` hands
  the terminal's environment to the daemon, so `ATLAS_EMACS_BACKGROUND`
  (else `COLORFGBG`, else Emacs's guess) and `ATLAS_EMACS_THEMES=off` are read
  per attach — no per-person config on the LLM side. Background is set via the
  terminal parameter (Emacs can't query a tty and guesses light). Both apply
  only in atlas daemons (socket `atlas-*`), never the person's own server.
- **Title bands right-align at display time** (`:align-to right`). Layouts
  render a pane before splitting its neighbour, so render-time widths are wrong.
- **Layouts refuse to draw while the human's minibuffer is open.**
- **All layout views use the stylesheet**: entity (details, data flow,
  dependents), blast radius (by depth + by type), data-key flow, domain survey,
  architecture (at a glance + execution order), home.
- **Blast radius groups by distance** and folds repeat paths into one count,
  instead of an indented BFS tree with `↑` duplicates.
- **Tab bar gets a dark variant** prepended to Emacs's own spec (light frames
  unchanged).
- **One script, no side helpers.** `repl`, `ensure`, `attach`, `eval`,
  `status`, `stop` share the Emacs lookup and socket logic, so the skill
  carries no binaries or paths. `repl` reads the cider-nrepl version from the
  person's installed CIDER.
- **Mismatches are visible, not fixed silently.** `status`/`list` show the
  project's git branch and the REPL's working directory; `ensure` warns when
  the REPL runs in another directory. After switching branches in one
  checkout, views show what the REPL has loaded until the REPL is reloaded.
- **No `(dev/refresh)`.** The daemon never mutates the human's REPL.

## Known gaps

- **Registry noise in execution order.** `:fn.ide/*` tooling entities are
  listed alongside app code (77 entries on pet-shop, most of them tooling).
- **Registry split.** Emacs shows the local REPL; MCP tools answer from the
  cloud. There's no Emacs equivalent of v3's `POST /api/v3/registry` switch.
- **Source pane lands on the registration**, not the implementing function.
- **No highlight primitive** (`show FILE LINE END`) for the LLM to point at code
  while explaining.
- **`atlas-slice`** (org narrative buffer) is not offered as a view.
- **`/atlas-view`** (browser) duplicates target resolution.
- **Registry noise.** `:fn.ide/*` tooling entities share the registry with the
  app and crowd the architecture overview and execution order.
- **Not an `:atlas/llm-prompt` yet.** Its real dependencies (the elisp layouts,
  the daemon script) aren't registry entities, so `consumers-of` couldn't see
  them. Register it once the Emacs layer is modeled.

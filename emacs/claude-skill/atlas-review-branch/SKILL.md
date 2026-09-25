---
name: atlas-review-branch
description: File-by-file review of a branch or PR in the human's Emacs (`em`) — own git worktree and REPL, an org notebook with per-file notes (what changed, what to look at, atlas registry impact), diff views where RET edits the file, and an optional "everything unstaged" mode to stage hunk by hunk into one commit.
argument-hint: <branch or PR number> [repo dir]
---

# Atlas: review a branch

The human reviews; you prepare, annotate and verify. The review happens in the
Emacs frame they attach with `em` (see the `atlas-emacs` skill for the daemon
model). Everything you write goes in a notebook the human reads next to the
diff — findings in chat are a summary, not the deliverable.

Scripts are in this skill's directory:

```bash
SKILL="$(readlink -f "<this skill's base directory>")"
ATLAS="$SKILL/../../atlas-llm-daemon.sh"      # the atlas-emacs daemon script
```

Per-review data lives outside every repo, in
`~/.local/state/atlas-emacs/reviews/<slug>.org` (+ `<slug>.commits.txt`).

---

## 1. Resolve what to review

- **PR number** → `gh pr view N --repo <owner/repo> --json headRefName,baseRefName,state,title`.
- **Branch** → check it exists locally or on the remote.
- **Base** = `git merge-base <default-branch> <branch>`. Note if the branch is
  **stacked** on another open PR (its diff then includes the other PR's commits) —
  say so and ask whether to review only the top commits (`BASE` = the lower PR's tip).
- Say what you found (commits, files, +/−, open PR?) before building anything big.

## 2. Worktree

Review in a worktree next to the repo, never in the human's checkout:

```bash
git -C <repo> worktree add <repo>-review-<slug> <branch>
```

If the branch is checked out elsewhere, reuse that worktree or use `--detach`.

**lsp:** if the human's Emacs uses lsp-mode, register the worktree as its own
folder in the daemon that will show the review, or lsp picks a parent folder
(and may prompt to watch thousands of directories):

```bash
$ATLAS eval --project <daemon project> '(lsp-workspace-folders-add "<worktree>")'
```

## 3. REPL for the worktree

Start one with `$ATLAS repl --project <worktree> [--aliases …] [--extra-paths …] [--boot …] --new`,
following the project's conventions (its memory notes say which aliases, paths
and boot form its dev tooling needs). The REPL gives the branch's registry for
the impact step and CIDER for branch code. It must not start the app: `repl`
always makes nREPL the main, whatever the aliases say — still confirm only the
nREPL port listens afterwards.

## 4. Read the change, then write the notebook

1. `git diff --stat <base>` and `git log --oneline <base>..HEAD` for the shape.
2. **Atlas impact** (only when the project has an atlas registry), in the
   worktree REPL:
   ```clojure
   (load-file "<SKILL>/scripts/impact.clj")
   (atlas-review.impact/report "<worktree>" "<base>")
   ```
   Per file: dev-ids it newly registers / no longer registers / still registers,
   each with type and dependent count. Also worth running: invariants the branch
   touches (e.g. a new invariant test → run that invariant in the REPL).
3. Read every source file's diff; skim tests for what they cover.
4. Scaffold, then fill:
   ```bash
   "$SKILL/scripts/scaffold-notebook.sh" <worktree> <base> "<branch>" ~/.local/state/atlas-emacs/reviews/<slug>.org
   ```
   - **Reorder** the file headings into reading order: the core change first,
     definitions before their callers, call sites, then tests.
   - Per file fill `What:` (the change in one or two lines), `Look at:` (what a
     reviewer should check — risks, behaviour changes, questions), `Atlas:`
     (from the impact report; omit when there's no registry). Nits last.
   - **Verify before you write a finding.** Read the code behind a suspicion; a
     wrong alarm costs the human more than a missing nit. Say "checked, not a
     bug" when you ruled something out.
   - The `Overview (not a file)` section: what the branch does, the headline
     risks, atlas-wide facts (new entities with many dependents, entities the
     registry can't see, invariant results).
   - Never touch `** Your notes`, TODO/DONE state or anything the human wrote.

## 5. Deliver to the human's frame

Use the daemon the human is attached to (`$ATLAS list` → the one with
`frames=1`), or ensure one for the worktree. `atlas-review.el` is loaded with
atlas; then:

```bash
$ATLAS eval --project <daemon project> \
  '(atlas-review/start "<worktree>" "<base>" "<notebook>")'
```

That opens the **index** (`review notes` tab): the notebook, one line per file.
Tell the human the keys:

| Key | Where | Does |
|---|---|---|
| `RET` | index | review that file: its diff (expanded) above its notes |
| `RET` | diff | edit the real file at that line |
| `C-c r b` | file | back to the diff (refreshed; saving refreshes too) |
| `C-c r r` | anywhere | reset the layout for the current file |
| `C-c r n` / `C-c r p` | anywhere | next / previous file |
| `C-c r d` | anywhere | toggle the file's TODO/DONE |
| `C-c r o` | anywhere | back to the index |
| `s` / `u` | diff | stage / unstage hunk or region (squash mode; new files go whole) |
| `C-c r t` | diff | show staged / unstaged changes |
| `C-c r s` | anywhere | magit status of the worktree (commit with `c c`) |

Tabs belong to a frame: after the human re-attaches, re-open with
`(atlas-review/notebook)` (and `(atlas-review/open atlas-review--current)`).

Verify what they see with `(atlas-layout/llm-screen)` — and count hunks against
`git diff`, since an empty diff pane is easy to miss.

## 6. Optional: everything unstaged, one final commit

Only when the human asks. The branch must be clean.

```bash
"$SKILL/scripts/squash-setup.sh" <worktree> <base> <branch>-squashed \
  ~/.local/state/atlas-emacs/reviews/<slug>.commits.txt
```

It keeps the original branch (and its PR) untouched, saves the commit messages,
unstages everything and marks new files intent-to-add. The review diff then
shows **unstaged** changes, so `s` makes a hunk leave the view: what's left is
what hasn't been accepted. Link the messages file from the notebook header.

To restart from scratch: `git reset -q` then `git add -N` the untracked files
(check first that the human has no edits or staged work they want to keep).

## 7. Finishing

When the human has committed: **ask** how to publish — force-push over the PR's
branch, or push a new branch and open a new PR. Never push, force-push or open
a PR unprompted. Then offer cleanup: remove the worktree
(`git worktree remove`), `(lsp-workspace-folders-remove "<worktree>")`, stop the
worktree REPL, keep or archive the notebook.

## Guardrails

- Never edit the human's files unless asked; when you do, the buffer reloads on
  its own — show the hunk you touched afterwards.
- Never run tests in a REPL the human uses; use a separate JVM.
- Before rewriting the notebook file, check its buffer has no unsaved edits and
  keep every note the human wrote.
- Keep the notebook and messages file out of every repo.

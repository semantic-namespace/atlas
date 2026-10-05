---
name: atlas-review-branch
description: Review a branch or PR for a human, in their Emacs — a page of numbered, tagged points to read (what changed for them, what to decide, what backs it), with the paths, hunks and registry facts kept in an index the LLM navigates on request; the registry diffed base vs branch.
---

# Atlas: review a branch

The human reviews; you prepare, annotate and verify. The review happens in the
Emacs frame they attach with `em` (see the `atlas-emacs` skill for the daemon
model). What you write is a **page to read**, not a list of files: the human is
the code's developer, but reads as a human — prose first, specifics only when
they ask for them. Findings in chat are a summary; the notebook is the deliverable.

Scripts are in this skill's directory:

```bash
SKILL="$(readlink -f "<this skill's base directory>")"
ATLAS="$SKILL/../../atlas-llm-daemon.sh"      # the atlas-emacs daemon script
```

Per-review data lives outside every repo, in
`~/.local/state/atlas-emacs/reviews/`: `<slug>.org` (the page the human reads),
`<slug>.index.org` (the evidence per point, yours), `<slug>.groupings.org` (every
hunk, grouped), `<slug>.commits.txt` (squash mode).

---

## 1. Resolve what to review

- **PR number** → `gh pr view N --repo <owner/repo> --json headRefName,baseRefName,state,title`.
- **Branch** → check it exists locally or on the remote.
- **Base** = `git merge-base <default-branch> <branch>`. Note if the branch is
  **stacked** on another open PR — say so and ask whether to review only the top
  commits (`BASE` = the lower PR's tip).
- If a notebook for this slug already exists, read it first: it may hold the
  human's notes. Reuse the worktree and the notebook unless the PR head moved.
- Say what you found (commits, files, +/−, open PR?) before building anything big,
  and ask **one** question: where are they nervous, what should be verified first?
  One line; don't block on it — start the worktrees while they answer. Their
  answer aims the verification better than any heuristic; without one, verify
  the residue (see 4.2) and the behaviour changes at call sites first.

## 2. Worktrees

Review in worktrees next to the repo, never in the human's checkout — one for the
branch and, when the project has an atlas registry, one for the base:

```bash
git -C <repo> worktree add <repo>-review-<slug> <branch>
git -C <repo> worktree add --detach <repo>-review-<slug>-base <base>
```

If the branch is checked out elsewhere, reuse that worktree or use `--detach`.

**lsp:** if the human's Emacs uses lsp-mode, register the branch worktree as its
own folder in the daemon that will show the review, or lsp picks a parent folder:

```bash
$ATLAS eval --project <daemon project> '(lsp-workspace-folders-add "<worktree>")'
```

## 3. REPLs: one, or two

Follow the project's conventions (its memory notes say which aliases, paths and
boot form its dev tooling needs — a project may have its own per-worktree registry
REPL script; prefer it). No REPL may start the app.

- **Full mode** — a REPL per worktree. The branch REPL answers "what does the
  branch's registry say"; the base REPL exists only to be diffed against. Use it
  when the diff touches a registration (`git diff <base> | grep -c "register!\|bind\|def-.*tool"`
  is non-zero), or when the human asked for the registry view.
- **Cheap mode** — the branch REPL only, no contract diff, no base worktree.
  For a change that registers nothing new: the hunk map and the invariants still
  run; the "Registry diff" facts are replaced by one line saying no contract
  changed (you checked the registrations, not the registry).
- Without a registry at all, one REPL for CIDER is enough.

## 4. Read the change, then write the notebook

1. `git diff --stat <base>` and `git log --oneline <base>..HEAD` for the shape.
2. **Registry diff and hunk map** (only with an atlas registry). Hunk owners and
   change paths come from [sdiff](https://github.com/semantic-namespace/diff)
   (a checkout at `~/git/semantic-namespace/diff`, or `SDIFF_HOME`, plus `bb`);
   without it they fall back to line overlap. On *each* REPL:
   ```clojure
   (load-file "<SKILL>/scripts/semantic.clj")
   (atlas-review.semantic/dump-registry! "<tmp>/registry-<base|branch>.edn")
   ```
   then on the branch REPL:
   ```clojure
   (def r (atlas-review.semantic/analyse "<worktree>" "<base>" "<tmp>/registry-base.edn" "<tmp>/registry-branch.edn"))
   (:cdiff r)                                                                  ; contracts that changed: new / removed / context, deps, response, aspects, MCP inputs
   (atlas-review.semantic/append-index! r "~/.local/state/atlas-emacs/reviews/<slug>.index.org")   ; evidence per moved data key
   (atlas-review.semantic/write-extended! r "~/.local/state/atlas-emacs/reviews/<slug>.groupings.org")
   ```
   What to read off it, beyond the obvious new/removed entities:
   - an entity whose **compound identity changed while its declaration did not**
     (inherited aspects through a new dep) — say so, it changes what Atlas answers;
   - a **derived entity** (an MCP tool generated from an exec-fn) whose required
     inputs changed because a context key was added without `optional-context`;
   - a key **produced but consumed by nobody**, or consumed in code without being
     declared — the registry will answer wrongly about it later;
   - the **residue**: hunks that touch no entity and no data key. The core of a
     change is often plain functions; a review must say when Atlas cannot see it.
   Also run the invariants the branch touches (a new invariant test → run that
   invariant in the branch REPL).
   `impact.clj` (per-file registered / unregistered / kept, with dependent counts)
   is still there when the file view is what the human asked for.
3. **Facts pass.** Read every source file's diff; skim tests for what they cover.
   Build the index first, point by point, before writing any prose: a suspicion
   goes in only once it is verified — evaluate the doubtful form in the branch
   REPL, read the code behind it. A wrong alarm costs the human more than a
   missing nit; say "checked, not a bug" when you ruled something out. Then
   ```clojure
   (atlas-review.semantic/stamp-index! "<worktree>" "<index>")
   ```
   so a later run can tell which points' evidence moved.
4. **Prose pass.** Scaffold, then write the page from the index — nothing on the
   page without a line in the index behind it, or the point is tagged `judge`.
   Write **The PR in one breath** last: by then you know what the PR is for.
   ```bash
   "$SKILL/scripts/scaffold-review.sh" <worktree> <base> "<slug>" ~/.local/state/atlas-emacs/reviews/<slug>.org
   ```
   **The page** (`<slug>.org`) — for the human, who is the code's developer but
   reads as a human: prose, no paths, no line numbers, no counts unless they *are*
   the point. In reading order:
   - **The PR in one breath** — a proposed title (the effect first, the mechanism
     second; the ticket id if any) and a paragraph: what was wrong or missing,
     what the branch does about it, what came along at the edge. The deleted
     comments and the old docstrings are often the clearest statement of the why.
     End with one sentence: what it needs before merging, naming the point numbers.
   - **Numbered sections**, one per thing that changed *for the reader*, grouped
     by what the registry diff moved (a data key, a contract, an identity), never
     by file. A sentence as title, a paragraph of prose, then **points** as
     sub-headings `** 2.2 <a sentence>` with a few lines each.
   - **Org tags** on every section and point, two families declared in the
     header: what it asks of the reader — `block`, `decide`, `small`, `fyi` — and
     what backs it — `atlas` (the registry), `code` (a line you can open), `repl`
     (a form you evaluated), `judge` (your reading, checked by nothing). Org then
     filters for free (`C-c / m block`). Every point carries at least one backing
     tag; a point that is only `judge` says so.
   - **Nits** last.
   **The index** (`<slug>.index.org`) — for you: one heading per point number,
   the evidence lines under it, each ending in its source tag, linking code as
   `[[file:<abs>::LINE][name:LINE]]` and hunks as `[[diff:<path rel. to worktree>::LINE][…]]`.
   Start from what `append-index!` generated (the `key/…` headings) and the
   findings you verified. The human never opens it; they ask for a number.
   - **The first line of a point is the thing to look at**: `point` opens its
     first link. For a `block`, that is the hunk where the fix goes, so the human
     goes `C-c r .` → `RET` (edit) → save → `s` (stage) without leaving the point.
   - When the contract diff moved no data key (a cache, a race, a query — the
     spine is empty), group the sections by the entities the hunks touch, then by
     the residue's owners (the vars), and say on the page that the registry saw
     none of it. Don't invent a data-flow story.
   - Never touch anything the human wrote in either file. Before rewriting,
     check the buffer has no unsaved edits.

## 5. Deliver to the human's frame

Use the daemon the human is attached to (`$ATLAS list` → the one with
`frames=1`), or ensure one for the worktree. `atlas-review.el` is loaded with
atlas; then:

```bash
$ATLAS eval --project <daemon project> \
  '(atlas-review/start "<worktree>" "<base>" "<notebook>")'
```

That opens the page in the **review notes** tab with the reading mode on
(markup hidden, tags coloured). Tell the human the keys, then **navigate for
them**: when they name a point, run `(atlas-review/point "2.2")` — it shows the
point's first target (the file at the line, or the diff at the hunk) in the
**review** tab with the evidence lines beneath, and returns those lines to you.
"Next", "the diff", "what does the registry say" are yours to answer from the
index and the branch REPL.

| Key | Where | Does |
|---|---|---|
| `TAB` | on a heading | fold / unfold |
| `C-c / m <tag>` | page | show only the points with that tag (`block`, `decide`, `judge`…) |
| `C-c r .` | anywhere | show what backs a point number |
| `RET` | on a link in the evidence pane | the file at that line, or the diff at that hunk |
| `RET` | diff | edit the real file at that line |
| `C-c r b` | file | back to the diff (refreshed; saving refreshes too) |
| `C-c r d` | on a heading | toggle the point's TODO/DONE |
| `C-c r o` | anywhere | back to the page |
| `s` / `u` | diff | stage / unstage hunk or region (squash mode; new files go whole) |
| `C-c r t` | diff | show staged / unstaged changes |
| `C-c r s` | anywhere | magit status of the worktree (commit with `c c`) |
| `M-x atlas-review-reading-mode` | page | toggle the raw org |

A file-by-file index (`scaffold-notebook.sh`, headings with `:REVIEW_FILE:`,
`RET` opens the file's diff, `C-c r n`/`C-c r p` step through files) remains
available when the human asks for that view.

**The loop.** The page is the conversation, not a report. The human marks points
DONE (`C-c r d`) and writes replies under a point as lines starting with `>`.
On every later turn — a reply, a re-attach, a pushed fix — start from
```elisp
(atlas-review/open-points)        ; ((id state notes) …): what is still open, and what they wrote
```
and, when the branch moved,
```clojure
(atlas-review.semantic/stale-points "<worktree>" "<index>")   ; points whose linked lines changed
```
Answer only the open points; re-verify the stale ones and rewrite their prose;
re-stamp. Never mark a point DONE yourself.

Tabs belong to a frame: after the human re-attaches, re-open with
`(atlas-review/notebook)`.

Verify what they see with `(atlas-layout/llm-screen)`, and show one point
yourself: an empty diff pane is easy to miss.

## 6. Optional: everything unstaged, one final commit

Only when the human asks. The branch must be clean.

```bash
"$SKILL/scripts/squash-setup.sh" <worktree> <base> <branch>-squashed \
  ~/.local/state/atlas-emacs/reviews/<slug>.commits.txt
```

It keeps the original branch (and its PR) untouched, saves the commit messages,
unstages everything and marks new files intent-to-add. Every review diff then
shows **unstaged** changes, so `s` makes a hunk leave the view: what's left is
what hasn't been accepted. Link the messages file from the notebook header.

To restart from scratch: `git reset -q` then `git add -N` the untracked files
(check first that the human has no edits or staged work they want to keep).

## 7. Finishing

When the human has committed: **ask** how to publish — force-push over the PR's
branch, or push a new branch and open a new PR. Never push, force-push or open
a PR unprompted. Then offer cleanup: remove both worktrees
(`git worktree remove`), `(lsp-workspace-folders-remove "<worktree>")`, stop the
REPLs, keep or archive the notebook.

## Guardrails

- Never edit the human's files unless asked; when you do, the buffer reloads on
  its own — show the hunk you touched afterwards.
- Never run tests in a REPL the human uses; use a separate JVM.
- Keep the page, the index, the groupings and the messages file out of every repo.
- The registry sees registrations, not meaning: a decision table written as plain
  functions is the PR's core and invisible to Atlas. Say it; don't hide it under
  a file heading.

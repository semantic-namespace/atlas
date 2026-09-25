#!/usr/bin/env bash
# Put a reviewed branch in "everything unstaged" form, so the human can stage
# hunk by hunk into one final commit.
#
#   squash-setup.sh WORKTREE BASE NEW-BRANCH MESSAGES-FILE
#
# Refuses unless the worktree is clean. Then:
#   1. saves every commit message BASE..HEAD to MESSAGES-FILE (for the final one)
#   2. creates NEW-BRANCH at HEAD — the original branch (and any PR) is untouched
#   3. `git reset BASE`: all changes become unstaged, the files are unchanged
#   4. `git add -N` on files that became untracked, so diffs still show them
#   5. checks the working tree is byte-identical to the original tip
# Undo: `git switch <original branch>` (NEW-BRANCH can then be deleted).

set -euo pipefail
[[ $# -eq 4 ]] || { sed -n '2,13p' "$0"; exit 2; }
WT="$(cd "$1" && pwd)"; BASE="$2"; NEW="$3"; MSGS="$4"
cd "$WT"
[[ -z "$(git status --porcelain)" ]] || { echo "ERROR: $WT has uncommitted changes; commit or stash them first" >&2; exit 1; }
orig_branch="$(git rev-parse --abbrev-ref HEAD)"
orig_tip="$(git rev-parse HEAD)"
mkdir -p "$(dirname "$MSGS")"
git log --reverse --format='### %h %s%n%n%b' "$BASE"..HEAD > "$MSGS"
git switch -q -c "$NEW"
git reset -q "$BASE"
mapfile -t new_files < <(git ls-files --others --exclude-standard)
(( ${#new_files[@]} )) && git add -N -- "${new_files[@]}"
[[ -z "$(git diff "$orig_tip")" ]] || { echo "ERROR: working tree differs from $orig_tip — check before continuing" >&2; exit 1; }
echo "branch $NEW (from $orig_branch @ ${orig_tip:0:9}), everything unstaged against $(git rev-parse --short "$BASE"):"
echo "  $(git status --short | wc -l) files ($(git status --short | grep -c '^ A' || true) new, intent-to-add)"
echo "  commit messages: $MSGS ($(grep -c '^### ' "$MSGS") commits)"
echo "  undo: git switch $orig_branch"

#!/usr/bin/env bash
# Write the review notebook skeleton and its index sidecar.
#
#   scaffold-review.sh WORKTREE BASE TITLE OUT.org
#
# The notebook is a page to read: an intro (proposed title, what the branch does
# and why, what it needs before merging — by point number), then numbered
# sections and points as org headings, each tagged with what it asks of the
# reader (block / decide / small / fyi) and what backs it (atlas / code / repl /
# judge). No links or paths in the page.
#
# The sidecar OUT.index.org holds, per point number, the evidence lines with
# their links (file:<abs>::LINE, diff:<rel>::LINE). The LLM reads it;
# `atlas-review/point' (C-c r .) shows a point's targets in Emacs.
# Refuses to overwrite an existing notebook: it may hold the human's notes.

set -euo pipefail
[[ $# -eq 4 ]] || { sed -n '2,15p' "$0"; exit 2; }
WT="$(cd "$1" && pwd)"; BASE="$2"; TITLE="$3"; OUT="$4"
[[ -e "$OUT" ]] && { echo "ERROR: $OUT exists — it may hold review notes; move it away first" >&2; exit 1; }
mkdir -p "$(dirname "$OUT")"
INDEX="${OUT%.org}.index.org"

cd "$WT"
tip="$(git rev-parse --short HEAD)"
commits="$(git rev-list --count "$BASE"..HEAD)"
total="$(git diff --shortstat "$BASE" | sed 's/^ *//')"

cat > "$OUT" <<EOF
#+TITLE: Review — $TITLE
#+STARTUP: overview
#+TODO: TODO | DONE
#+TAGS: block(b) decide(d) small(s) fyi(f) | atlas(a) code(c) repl(r) judge(j)

$TITLE ($tip) · $commits commits · base $(git rev-parse --short "$BASE") · $total. Numbered points; the tags say what kind of point it is and what backs it. Ask for any number to see the code, the diff or the registry behind it.

* The PR in one breath
Proposed title: /…/

(Write this last. What was wrong or missing, what the branch does about it, what came along at the edge. Then one sentence: what it needs before merging, by point number.)

* 1. …                                                                  :decide:
(What changed for the reader, in a paragraph.)
** 1.1 …                                                             :code:atlas:
(One point: a sentence as title, a few lines of prose. No paths, no line numbers.)

* Nits                                                                   :small:
- …
EOF

[[ -e "$INDEX" ]] || cat > "$INDEX" <<EOF
#+TITLE: Index — $TITLE
# One heading per numbered point of the notebook. The LLM reads this; the human asks for a number.
# Links: file:<abs>::LINE opens the code line; diff:<path relative to the worktree>::LINE opens the review diff at that hunk.

* 1.1
- … ‹code›
EOF
echo "notebook: $OUT"
echo "index:    $INDEX"

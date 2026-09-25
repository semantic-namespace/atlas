#!/usr/bin/env bash
# Write the review notebook skeleton for a worktree: one TODO heading per file
# changed against BASE, in the format atlas-review.el reads.
#
#   scaffold-notebook.sh WORKTREE BASE TITLE OUT.org
#
# Each heading: "* TODO <file name>  +added −removed  · …/<last two dirs>",
# a REVIEW_FILE property with the path relative to the worktree (org's FILE
# property is reserved, hence the name), an `atlas-review:' link, empty
# What / Look at / Atlas lines for the LLM to fill, and "** Your notes".
# Refuses to overwrite an existing notebook: it may hold the human's notes.

set -euo pipefail
[[ $# -eq 4 ]] || { sed -n '2,11p' "$0"; exit 2; }
WT="$(cd "$1" && pwd)"; BASE="$2"; TITLE="$3"; OUT="$4"
[[ -e "$OUT" ]] && { echo "ERROR: $OUT exists — it may hold review notes; move it away first" >&2; exit 1; }
mkdir -p "$(dirname "$OUT")"

cd "$WT"
branch="$(git rev-parse --abbrev-ref HEAD)"
tip="$(git rev-parse --short HEAD)"
commits="$(git rev-list --count "$BASE"..HEAD)"
# Working tree against BASE: covers committed, staged and unstaged changes
# (intent-to-add files included).
mapfile -t rows < <(git diff --numstat "$BASE")
declare -A status
while IFS=$'\t' read -r st path _; do status["$path"]="$st"; done < <(git diff --name-status "$BASE")
files=${#rows[@]}
total="$(git diff --shortstat "$BASE" | sed 's/^ *//')"

{
  echo "#+TITLE: Review — $TITLE"
  echo "#+STARTUP: overview"
  echo "#+TODO: TODO | DONE"
  echo
  echo "Worktree: $WT · branch $branch ($tip) · base $(git rev-parse --short "$BASE") · $commits commits · $total"
  echo "Keys: =RET= on a file (here): review it · in the diff: =RET= edit at that line, =s= / =u= stage / unstage (squash review) · =C-c r b= back to the diff · =C-c r r= reset the layout · =C-c r t= staged / unstaged · =C-c r n= / =C-c r p= next / previous · =C-c r d= toggle DONE · =C-c r s= magit status · =C-c r o= this index"
  echo "* Overview (not a file)"
  echo "- "
  for row in "${rows[@]}"; do
    IFS=$'\t' read -r add del path <<<"$row"
    name="$(basename "$path")"; dir="$(dirname "$path")"
    IFS='/' read -ra parts <<<"$dir"
    if (( ${#parts[@]} > 2 )); then where="…/${parts[-2]}/${parts[-1]}"; else where="$dir"; fi
    if [[ "${status[$path]:-}" == A ]]; then size="+$add new"
    elif [[ "$add" == "-" ]]; then size="binary"
    else size="+$add −$del"; fi
    echo "* TODO $name  $size  · $where"
    echo ":PROPERTIES:"
    echo ":REVIEW_FILE: $path"
    echo ":END:"
    echo "[[atlas-review:$path][open]]"
    echo "- What: "
    echo "- Look at: "
    echo "- Atlas: "
    echo "** Your notes"
  done
} > "$OUT"
echo "notebook: $OUT ($files files)"

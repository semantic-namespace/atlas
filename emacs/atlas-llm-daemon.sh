#!/usr/bin/env bash
# EXPERIMENTAL — Emacs daemon an LLM drives while a human watches.
#
# The LLM runs `ensure`; the human attaches a terminal frame with `attach`
# from another terminal.  Layouts (atlas-layout.el) are drawn into that frame.
#
# Usage:
#   atlas-llm-daemon.sh repl   [--project DIR] [--aliases :a:b]  start a project nREPL with CIDER middleware
#   atlas-llm-daemon.sh ensure [--project DIR] [--port N]        start/reuse daemon, connect CIDER
#   atlas-llm-daemon.sh attach [--project DIR]                   (human) open a terminal frame
#   atlas-llm-daemon.sh eval   [--project DIR] FORM              eval elisp in the daemon, print result
#   atlas-llm-daemon.sh status [--project DIR]
#   atlas-llm-daemon.sh stop   [--project DIR]
#   atlas-llm-daemon.sh install-skill [--user]                   link the /atlas-emacs Claude Code skill
#
# DIR defaults to the current directory; one daemon per project, on socket
# atlas-<basename of DIR>.  Suggested alias:  alias em='<repo>/emacs/atlas-llm-daemon.sh attach'
#
# Port resolution (ensure, when --port is omitted):
#   1. DIR/.nrepl-port, if that port answers
#   2. the single REPL `clj-nrepl-eval --discover-ports` reports for DIR
#   otherwise it fails and lists candidates — it never guesses.
#
# Personal preferences travel with `attach` (emacsclient -t passes the
# terminal's environment to the daemon), so each person sets them once in
# their own shell profile:
#   ATLAS_EMACS_BACKGROUND=dark|light   terminal background (default: COLORFGBG
#                                        hint, else Emacs's own guess)
#   ATLAS_EMACS_THEMES=off              disable your Emacs themes in this daemon
#                                        (they're usually picked for a GUI frame)
#
# Env for the tools themselves:
#   EMACS / EMACSCLIENT         binaries (default: first Emacs >= 27 on PATH or
#                               in /snap/bin; emacsclient from the same dir)
#   ATLAS_CIDER_NREPL_VERSION   cider-nrepl for `repl` (default: the version your
#                               installed CIDER requires)

set -euo pipefail

ATLAS_EMACS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SELF="$ATLAS_EMACS_DIR/$(basename "${BASH_SOURCE[0]}")"

# First Emacs >= 27 (tab-bar) among: $EMACS, PATH, /snap/bin. A distro emacs
# on PATH is often much older than the one the user actually runs.
pick_emacs() {
  local e v
  for e in ${EMACS:-} "$(command -v emacs || true)" /snap/bin/emacs; do
    [[ -x "$e" ]] || continue
    v="$("$e" --version 2>/dev/null | grep -oP 'GNU Emacs \K[0-9]+' || echo 0)"
    (( v >= 27 )) && { echo "$e"; return; }
  done
  echo "ERROR: no Emacs >= 27 found; set EMACS=/path/to/emacs" >&2; exit 1
}
EMACS="$(pick_emacs)"
CLIENT="${EMACSCLIENT:-$(dirname "$EMACS")/emacsclient}"

cmd="${1:-}"; shift || true
PROJECT="$PWD"; PORT=""; ALIASES=""; FORM=""; USER_SCOPE=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --project) PROJECT="$2"; shift 2 ;;
    --port)    PORT="$2";    shift 2 ;;
    --aliases) ALIASES="$2"; shift 2 ;;
    --user)    USER_SCOPE=1; shift ;;
    --*) echo "unknown argument: $1" >&2; exit 2 ;;
    *) FORM="$1"; shift ;;
  esac
done
PROJECT="$(cd "$PROJECT" && pwd)"
NAME="atlas-$(basename "$PROJECT")"
# Always an absolute socket path: snap-confined emacs/emacsclient disagree on
# the default socket dir, so name-only sockets are not found.
SOCKDIR="${XDG_RUNTIME_DIR:+$XDG_RUNTIME_DIR/emacs}"; SOCKDIR="${SOCKDIR:-/tmp/emacs$(id -u)}"
SOCKET="$SOCKDIR/$NAME"
STATEDIR="${XDG_STATE_HOME:-$HOME/.local/state}/atlas-emacs"

ec() { timeout 20 "$CLIENT" --socket-name="$SOCKET" --eval "$1" 2>/dev/null; }
daemon_up() { ec "t" >/dev/null; }
# Capture, then match: `cmd | grep -q` fails under pipefail when grep exits
# early and cmd gets SIGPIPE writing its remaining output.
port_alive() { local out; out="$(clj-nrepl-eval -p "$1" --timeout 3000 "1" 2>/dev/null || true)"; [[ "$out" == *"=> 1"* ]]; }
# Evaluate FORM in the daemon and print its value as plain text (strings
# verbatim, anything else printed). emacsclient's own output is prin1 with
# escapes, which can't be reliably turned back into text, so the daemon writes
# the value to a file next to its socket (a directory both sides can reach,
# even for snap-confined Emacs) and we print that.
eval_text() {
  local out tmp
  tmp="$SOCKDIR/.$NAME-eval.$$"
  out="$(timeout 30 "$CLIENT" --socket-name="$SOCKET" --eval \
         "(let ((v $1)) (with-temp-file \"$tmp\" (insert (if (stringp v) v (prin1-to-string v)))) t)" 2>&1)" \
    || { rm -f "$tmp"; echo "$out" >&2; return 1; }
  cat "$tmp"; echo; rm -f "$tmp"
}

resolve_port() {
  if [[ -n "$PORT" ]]; then
    port_alive "$PORT" || { echo "ERROR: nREPL port $PORT does not answer" >&2; exit 1; }
    return
  fi
  if [[ -f "$PROJECT/.nrepl-port" ]]; then
    local p; p="$(cat "$PROJECT/.nrepl-port")"
    if port_alive "$p"; then PORT="$p"; return; fi
    echo "note: $PROJECT/.nrepl-port ($p) is stale" >&2
  fi
  local matches
  matches="$(clj-nrepl-eval --discover-ports 2>/dev/null \
             | grep -E -- "- $PROJECT\$" \
             | grep -oP 'localhost:\K[0-9]+' || true)"
  local n; n="$(echo -n "$matches" | grep -c . || true)"
  if [[ "$n" == 1 ]]; then PORT="$matches"; return; fi
  echo "ERROR: $n nREPL servers match $PROJECT — start one with \`$(basename "$SELF") repl\`, or pass --port. Candidates:" >&2
  clj-nrepl-eval --discover-ports >&2 || true
  exit 1
}

# cider-nrepl version the user's CIDER expects: env, then the running daemon,
# then cider.el in the usual package dirs (package.el, straight, elpaca).
cider_nrepl_version() {
  local v=""
  [[ -n "${ATLAS_CIDER_NREPL_VERSION:-}" ]] && { echo "$ATLAS_CIDER_NREPL_VERSION"; return; }
  if daemon_up; then
    v="$(ec "(and (require 'cider nil t) cider-required-middleware-version)" | tr -d '"')"
    [[ "$v" =~ ^[0-9] ]] && { echo "$v"; return; }
  fi
  v="$(find "$HOME/.emacs.d" "$HOME/.config/emacs" -name cider.el -path '*cider*' 2>/dev/null \
        | xargs -r grep -hoP '\(defconst cider-required-middleware-version\s+"\K[^"]+' 2>/dev/null \
        | sort -V | tail -1 || true)"
  [[ -n "$v" ]] && { echo "$v"; return; }
  echo "ERROR: can't find your CIDER's required cider-nrepl version; set ATLAS_CIDER_NREPL_VERSION" >&2
  exit 1
}

case "$cmd" in
  repl)
    if [[ -f "$PROJECT/.nrepl-port" ]] && port_alive "$(cat "$PROJECT/.nrepl-port")"; then
      echo "repl: already running on port $(cat "$PROJECT/.nrepl-port")"; exit 0
    fi
    [[ -f "$PROJECT/deps.edn" ]] || { echo "ERROR: no deps.edn in $PROJECT" >&2; exit 1; }
    ver="$(cider_nrepl_version)"
    if [[ -z "$ALIASES" ]] && grep -q ':repl' "$PROJECT/deps.edn"; then ALIASES=":repl"; fi
    mkdir -p "$STATEDIR"
    log="$STATEDIR/$NAME-nrepl.log"
    rm -f "$PROJECT/.nrepl-port"
    echo "Starting nREPL in $PROJECT (cider-nrepl $ver${ALIASES:+, aliases $ALIASES}); log: $log"
    (cd "$PROJECT" && nohup clojure \
       -Sdeps "{:deps {nrepl/nrepl {:mvn/version \"1.3.0\"} cider/cider-nrepl {:mvn/version \"$ver\"}}}" \
       "-M${ALIASES}" -m nrepl.cmdline \
       --middleware '[cider.nrepl/cider-middleware]' --port 0 >"$log" 2>&1 &)
    # JVM start + first-time dependency download can take a while; report
    # progress (with the log's last line) so a slow start doesn't look frozen.
    start=$SECONDS; next=$((SECONDS + 10)); ready=""
    while (( SECONDS - start < 120 )); do
      if [[ -f "$PROJECT/.nrepl-port" ]] && port_alive "$(cat "$PROJECT/.nrepl-port")"; then
        ready=1; break
      fi
      if (( SECONDS >= next )); then
        echo "  waiting for nREPL… $((SECONDS - start))s  [$(tail -n1 "$log" 2>/dev/null | cut -c1-80)]"
        next=$((SECONDS + 10))
      fi
      sleep 1
    done
    if [[ -z "$ready" ]]; then
      echo "ERROR: nREPL not answering after 120s. Last log lines ($log):" >&2
      tail -n5 "$log" >&2
      exit 1
    fi
    echo "repl: port $(cat "$PROJECT/.nrepl-port") (ready in $((SECONDS - start))s)"
    ;;
  ensure)
    resolve_port
    if ! daemon_up; then
      echo "Starting daemon '$NAME' in $PROJECT ..."
      mkdir -p "$SOCKDIR" && chmod 700 "$SOCKDIR"
      "$EMACS" --daemon="$SOCKET" --chdir "$PROJECT"
      for _ in $(seq 1 60); do daemon_up && break; sleep 1; done
      daemon_up || { echo "ERROR: daemon did not come up" >&2; exit 1; }
    fi
    ec "(progn (add-to-list 'load-path \"$ATLAS_EMACS_DIR\")
               (require 'atlas)
               (xterm-mouse-mode 1)
               t)" >/dev/null
    state="$(ec "(atlas-layout/llm-connect \"localhost\" $PORT \"$PROJECT/\")")"
    for _ in $(seq 1 30); do
      [[ "$(ec "(atlas-layout/llm-status)" || true)" == *"cider=localhost:$PORT"* ]] && break
      sleep 1
    done
    echo "connect: $state"
    # Dependency views read dep keys from registered ontologies; example
    # registries (e.g. app.pet-shop/init-registry!) don't load them, and then
    # every dependents pane is silently empty.
    onts="$(clj-nrepl-eval -p "$PORT" --timeout 10000 \
            "(do (require 'atlas.ontology) (count (atlas.ontology/all-ontologies)))" 2>/dev/null \
            | grep -oP '=> \K[0-9]+' || echo 0)"
    echo "ontologies: $onts"
    if (( onts < 5 )); then
      echo "WARNING: core ontologies not loaded — dependency views will be empty. In the REPL:" >&2
      echo "  (doseq [n '[atlas.ontology.execution-function atlas.ontology.interface-endpoint atlas.ontology.structure-component atlas.ontology.data-schema atlas.ontology.interface-protocol]] (require n :reload)) (atlas.datalog/reset-db-cache!)" >&2
    fi
    echo "status: $(eval_text "(atlas-layout/llm-status)")"
    echo "attach: $SELF attach --project $PROJECT"
    ;;
  attach)
    if ! daemon_up; then
      echo "No atlas daemon for $PROJECT ($SOCKET). Ask your LLM to run: $(basename "$SELF") ensure --project $PROJECT" >&2
      exit 1
    fi
    exec env TERM="${TERM:-xterm-256color}" "$CLIENT" -t -s "$SOCKET"
    ;;
  eval)
    [[ -n "$FORM" ]] || { echo "usage: $(basename "$SELF") eval [--project DIR] FORM" >&2; exit 2; }
    daemon_up || { echo "ERROR: no daemon at $SOCKET — run ensure first" >&2; exit 1; }
    eval_text "$FORM"
    ;;
  status)
    if daemon_up; then eval_text "(atlas-layout/llm-status)"; else echo "socket=$SOCKET down"; fi
    ;;
  stop)
    daemon_up && ec "(kill-emacs)" >/dev/null || true
    echo "stopped $SOCKET"
    ;;
  install-skill)
    # Symlink, not copy: the skill stays in sync with the code it drives.
    # Default: this repo's .claude/skills (Claude Code sessions opened here);
    # --user: ~/.claude/skills (every session).
    src="$ATLAS_EMACS_DIR/claude-skill/atlas-emacs"
    if [[ -n "$USER_SCOPE" ]]; then dest_dir="$HOME/.claude/skills"
    else dest_dir="$(cd "$ATLAS_EMACS_DIR/.." && pwd)/.claude/skills"; fi
    dest="$dest_dir/atlas-emacs"
    mkdir -p "$dest_dir"
    if [[ -L "$dest" && "$(readlink -f "$dest")" == "$(readlink -f "$src")" ]]; then
      echo "skill: already linked at $dest"; exit 0
    fi
    [[ -e "$dest" ]] && { echo "ERROR: $dest exists and is not a link to $src — move it away first" >&2; exit 1; }
    ln -s "$src" "$dest"
    echo "skill: linked $dest -> $src (use /atlas-emacs in Claude Code)"
    ;;
  *)
    sed -n '2,/^set -euo/p' "$0" | sed '$d'; exit 2 ;;
esac

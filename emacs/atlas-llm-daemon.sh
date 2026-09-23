#!/usr/bin/env bash
# EXPERIMENTAL — Emacs daemon an LLM drives while a human watches.
#
# The LLM runs `ensure`; the human attaches from another terminal with the
# printed `emacsclient -t -s <socket>` command.  Layouts (atlas-layout.el)
# are drawn into that attached frame.
#
# Usage:
#   atlas-llm-daemon.sh ensure --project DIR [--port N] [--socket NAME] [--keep-themes]
#   atlas-llm-daemon.sh status [--project DIR] [--socket NAME]
#   atlas-llm-daemon.sh stop   [--project DIR] [--socket NAME]
#
# Port resolution (ensure, when --port is omitted):
#   1. DIR/.nrepl-port, if that port answers
#   2. the single REPL `clj-nrepl-eval --discover-ports` reports for DIR
#   otherwise it fails and lists candidates — it never guesses.
#
# Themes: ensure disables the user's themes inside this daemon (they're chosen
# for their GUI Emacs, usually light); atlas faces then sit on the terminal's
# own background.  --keep-themes leaves them on.  Only this daemon is affected.
#
# Env: EMACS / EMACSCLIENT override the binaries (default: first Emacs >= 27
#      on PATH or in /snap/bin; emacsclient from the same directory).

set -euo pipefail

ATLAS_EMACS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
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
PROJECT="$PWD"; PORT=""; SOCKET=""; KEEP_THEMES=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --project) PROJECT="$2"; shift 2 ;;
    --port)    PORT="$2";    shift 2 ;;
    --socket)  SOCKET="$2";  shift 2 ;;
    --keep-themes) KEEP_THEMES=1; shift ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done
PROJECT="$(cd "$PROJECT" && pwd)"
SOCKET="${SOCKET:-atlas-$(basename "$PROJECT")}"
# Always an absolute socket path: snap-confined emacs/emacsclient disagree on
# the default socket dir, so name-only sockets are not found.
if [[ "$SOCKET" != /* ]]; then
  SOCKDIR="${XDG_RUNTIME_DIR:+$XDG_RUNTIME_DIR/emacs}"; SOCKDIR="${SOCKDIR:-/tmp/emacs$(id -u)}"
  mkdir -p "$SOCKDIR" && chmod 700 "$SOCKDIR"
  SOCKET="$SOCKDIR/$SOCKET"
fi

ec() { timeout 20 "$CLIENT" --socket-name="$SOCKET" --eval "$1" 2>/dev/null; }
daemon_up() { ec "t" >/dev/null; }
port_alive() { clj-nrepl-eval -p "$1" --timeout 3000 "1" 2>/dev/null | grep -q '=> 1'; }

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
             | grep -F -- "- $PROJECT" | grep -E -- "- $PROJECT\$" \
             | grep -oP 'localhost:\K[0-9]+' || true)"
  local n; n="$(echo -n "$matches" | grep -c . || true)"
  if [[ "$n" == 1 ]]; then PORT="$matches"; return; fi
  echo "ERROR: $n nREPL servers match $PROJECT — pass --port. Candidates:" >&2
  clj-nrepl-eval --discover-ports >&2 || true
  exit 1
}

case "$cmd" in
  ensure)
    resolve_port
    if ! daemon_up; then
      echo "Starting daemon '$SOCKET' in $PROJECT ..."
      "$EMACS" --daemon="$SOCKET" --chdir "$PROJECT"
      for _ in $(seq 1 60); do daemon_up && break; sleep 1; done
      daemon_up || { echo "ERROR: daemon did not come up" >&2; exit 1; }
    fi
    ec "(progn (add-to-list 'load-path \"$ATLAS_EMACS_DIR\")
               (require 'atlas)
               (xterm-mouse-mode 1)
               t)" >/dev/null
    if [[ -z "$KEEP_THEMES" ]]; then
      ec "(progn (mapc #'disable-theme custom-enabled-themes) t)" >/dev/null
    fi
    state="$(ec "(atlas-layout/llm-connect \"localhost\" $PORT \"$PROJECT/\")")"
    for _ in $(seq 1 30); do
      ec "(atlas-layout/llm-status)" | grep -q "cider=localhost:$PORT" && break
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
    echo "status: $(ec "(atlas-layout/llm-status)")"
    echo "attach: $CLIENT -t -s $SOCKET"
    ;;
  status)
    if daemon_up; then ec "(atlas-layout/llm-status)"; else echo "socket=$SOCKET down"; fi
    ;;
  stop)
    daemon_up && ec "(kill-emacs)" >/dev/null || true
    echo "stopped $SOCKET"
    ;;
  *)
    sed -n '2,/^set -euo/p' "$0" | sed '$d'; exit 2 ;;
esac

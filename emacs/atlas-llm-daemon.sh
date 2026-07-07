#!/usr/bin/env bash
# EXPERIMENTAL — example script; edit the paths before use.
#
# Start (or reuse) the atlas-llm Emacs daemon and connect CIDER to the live nREPL.
# Usage: ./atlas-llm-daemon.sh [nrepl-port]
#   nrepl-port defaults to auto-discovery via clj-nrepl-eval.
#
# NOTE: this script hardcodes machine-specific paths (the EMACS/CLIENT binaries
# below, the atlas emacs/ load-path, and the CIDER project directory). Treat it
# as a template for your own setup rather than something to run as-is.

set -e

SOCKET=atlas-llm
EMACS=/snap/bin/emacs
CLIENT=/snap/bin/emacsclient

# Discover nREPL port unless given explicitly
if [[ -n "$1" ]]; then
  PORT=$1
else
  PORT=$(clj-nrepl-eval --discover-ports 2>/dev/null \
         | grep -oP 'localhost:\K[0-9]+' | head -1)
fi

if [[ -z "$PORT" ]]; then
  echo "ERROR: no nREPL server found. Start a REPL first, or pass the port as an argument."
  exit 1
fi

echo "nREPL port: $PORT"

# Start daemon if not already running
if ! $CLIENT --socket-name="$SOCKET" --eval "t" &>/dev/null; then
  echo "Starting atlas-llm daemon..."
  $EMACS --daemon="$SOCKET"
  echo "Waiting for init..."
  for i in $(seq 1 30); do
    sleep 1
    if $CLIENT --socket-name="$SOCKET" --eval "t" &>/dev/null; then
      echo "Daemon ready (${i}s)"
      break
    fi
  done
fi

# Load atlas-layout
echo "Loading atlas-layout..."
$CLIENT --socket-name="$SOCKET" --eval \
  "(progn (add-to-list 'load-path \"/home/tangrammer/git/semantic-namespace/atlas/emacs\")
          (require 'atlas-layout))"

# Connect CIDER (skip if already connected to avoid the "new session?" prompt)
ALREADY=$($CLIENT --socket-name="$SOCKET" --eval "(if (cider-connected-p) \"yes\" \"no\")" 2>/dev/null)
if [[ "$ALREADY" == '"yes"' ]]; then
  echo "CIDER already connected."
else
  echo "Connecting CIDER to localhost:$PORT..."
  $CLIENT --socket-name="$SOCKET" --eval \
    "(let ((default-directory \"/home/tangrammer/git/ruca/yorba/yorba-clj/\"))
       (cider-connect (list :host \"localhost\" :port $PORT
                            :project-dir \"/home/tangrammer/git/ruca/yorba/yorba-clj/\")))"

  echo "Waiting for CIDER connection..."
  for i in $(seq 1 15); do
    sleep 1
    CONNECTED=$($CLIENT --socket-name="$SOCKET" --eval \
      "(if (cider-connected-p) \"yes\" \"no\")" 2>/dev/null)
    if [[ "$CONNECTED" == '"yes"' ]]; then
      echo "CIDER connected (${i}s)"
      break
    fi
  done

  if [[ "$CONNECTED" != '"yes"' ]]; then
    echo "WARNING: CIDER did not connect in time."
    exit 1
  fi
fi

# Refresh changed namespaces and verify registry
echo "Refreshing namespaces..."
$CLIENT --socket-name="$SOCKET" --eval \
  "(cider-interactive-eval \"(dev/refresh)\")"
sleep 6

echo "Verifying registry..."
for i in 1 2 3 4; do
  COUNT=$(clj-nrepl-eval -p "$PORT" "(count @atlas.registry/registry)" 2>/dev/null | grep -oP '=> \K[0-9]+')
  [[ -n "$COUNT" ]] && break
  sleep 2
done
echo "Registry ready: ${COUNT:-?} entities"

echo "Done. Call layouts with:"
echo "  $CLIENT --socket-name=$SOCKET --eval '(atlas-layout/domain-survey \":domain/auth\")'"

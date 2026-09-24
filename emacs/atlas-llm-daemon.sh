#!/usr/bin/env bash
# EXPERIMENTAL — Emacs daemon an LLM drives while a human watches.
#
# The LLM runs `ensure`; the human attaches a terminal frame with `attach`
# from another terminal.  Layouts (atlas-layout.el) are drawn into that frame.
#
# Usage:
#   atlas-llm-daemon.sh repl   [--project DIR] [--aliases :a:b] [--extra-paths p1,p2] [--boot FORM]
#                                                                start a project nREPL with CIDER middleware
#   atlas-llm-daemon.sh ensure [--project DIR] [--port N]        start/reuse daemon, connect CIDER
#   atlas-llm-daemon.sh attach [--project DIR]                   (human) open a terminal frame
#   atlas-llm-daemon.sh eval   [--project DIR] FORM              eval elisp in the daemon, print result
#   atlas-llm-daemon.sh status [--project DIR]                   daemon, project, git branch, REPL and its directory
#   atlas-llm-daemon.sh list                                     every atlas daemon on this machine
#   atlas-llm-daemon.sh stop   [--project DIR | --socket PATH]
#   atlas-llm-daemon.sh install-skill [--user]                   link the /atlas-emacs Claude Code skill
#
# DIR defaults to the current directory. One daemon per project directory, on
# socket atlas-<basename>-<hash of the full path>, so two checkouts with the
# same folder name (e.g. worktrees) never share a daemon. Each daemon talks to
# exactly one REPL.  Suggested alias:  alias em='<repo>/emacs/atlas-llm-daemon.sh attach'
#
# Port resolution (ensure, when --port is omitted):
#   1. DIR/.nrepl-port, if that port answers
#   2. the single REPL `clj-nrepl-eval --discover-ports` reports for DIR
#   otherwise it fails and lists candidates — it never guesses.
#
# repl: uses the project's :repl alias if it has one (or --aliases). For projects
# whose dev alias runs a -main (starting the app), pass the dev source dirs with
# --extra-paths instead of the alias. --boot FORM is evaluated once the REPL is
# up (e.g. to load the registry). An existing REPL without CIDER's middleware
# doesn't count as running: a new one is started.
#
# Personal preferences travel with `attach` (emacsclient -t passes the
# terminal's environment to the daemon). Set them once in
# ${XDG_CONFIG_HOME:-~/.config}/atlas-emacs/env (KEY=VALUE lines), which every
# `attach` reads, so any attach command works; environment variables override:
#   ATLAS_EMACS_BACKGROUND=dark|light   terminal background (default: COLORFGBG
#                                        hint, else Emacs's own guess)
#   ATLAS_EMACS_THEMES=off              disable your Emacs themes in this daemon
#                                        (they're usually picked for a GUI frame)
#
# --project defaults to the git root of the current directory (else the
# directory itself). `ensure` keeps a daemon's current REPL while it's alive;
# pass --port to switch.
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
PROJECT="$(git rev-parse --show-toplevel 2>/dev/null || echo "$PWD")"; PORT=""; ALIASES=""; FORM=""; USER_SCOPE=""; SOCKET_ARG=""; EXTRA_PATHS=""; BOOT=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --project) PROJECT="$2"; shift 2 ;;
    --port)    PORT="$2";    shift 2 ;;
    --aliases) ALIASES="$2"; shift 2 ;;
    --extra-paths) EXTRA_PATHS="$2"; shift 2 ;;
    --boot)    BOOT="$2"; shift 2 ;;
    --user)    USER_SCOPE=1; shift ;;
    --socket)  SOCKET_ARG="$2"; shift 2 ;;
    --*) echo "unknown argument: $1" >&2; exit 2 ;;
    *) FORM="$1"; shift ;;
  esac
done
PROJECT="$(cd "$PROJECT" && pwd)"
NAME="atlas-$(basename "$PROJECT")-$(printf '%s' "$PROJECT" | sha1sum | cut -c1-6)"
# Always an absolute socket path: snap-confined emacs/emacsclient disagree on
# the default socket dir, so name-only sockets are not found.
SOCKDIR="${XDG_RUNTIME_DIR:+$XDG_RUNTIME_DIR/emacs}"; SOCKDIR="${SOCKDIR:-/tmp/emacs$(id -u)}"
SOCKET="${SOCKET_ARG:-$SOCKDIR/$NAME}"
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

git_branch() { git -C "$1" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "-"; }
# Working directory of the REPL on port $1 ("" if it doesn't answer).
repl_dir() {
  local out; out="$(clj-nrepl-eval -p "$1" --timeout 5000 '(System/getProperty "user.dir")' 2>/dev/null || true)"
  [[ "$out" =~ \=\>\ \"([^\"]*)\" ]] && echo "${BASH_REMATCH[1]}"
}
# llm-status plus what only the shell can see: the project's git branch and the
# connected REPL's working directory (it can differ from the project).
full_status() {
  local st port dir project
  st="$(eval_text "(atlas-layout/llm-status)")" || return 1
  port="$( [[ "$st" =~ cider=localhost:([0-9]+) ]] && echo "${BASH_REMATCH[1]}" )"
  project="$( [[ "$st" =~ project=([^ ]+) ]] && echo "${BASH_REMATCH[1]%/}" )"
  dir="$( [[ -n "$port" ]] && repl_dir "$port" )"
  echo "$st branch=$( [[ -n "$project" && "$project" != "-" ]] && git_branch "$project" || echo -) repl-dir=${dir:--}"
}

# Does the REPL on port $1 have CIDER's middleware loaded?
has_cider_middleware() {
  local out; out="$(clj-nrepl-eval -p "$1" --timeout 5000 "(boolean (try (requiring-resolve 'cider.nrepl.version/version) (catch Throwable _ nil)))" 2>/dev/null || true)"
  [[ "$out" == *"=> true"* ]]
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
      existing="$(cat "$PROJECT/.nrepl-port")"
      if has_cider_middleware "$existing"; then
        echo "repl: already running on port $existing"; exit 0
      fi
      echo "note: the REPL on port $existing has no CIDER middleware; starting another (it keeps running)"
    fi
    [[ -f "$PROJECT/deps.edn" ]] || { echo "ERROR: no deps.edn in $PROJECT" >&2; exit 1; }
    ver="$(cider_nrepl_version)"
    # Only an exact :repl alias (not e.g. :repl/clerk).
    if [[ -z "$ALIASES" ]] && grep -qE '(^|[[:space:]{]):repl[[:space:]]' "$PROJECT/deps.edn"; then ALIASES=":repl"; fi
    sdeps_aliases=""
    if [[ -n "$EXTRA_PATHS" ]]; then
      paths="$(printf '%s' "$EXTRA_PATHS" | tr ',' '\n' | sed 's/.*/"&"/' | tr '\n' ' ')"
      sdeps_aliases=" :aliases {:atlas-emacs-repl {:extra-paths [$paths]}}"
      ALIASES=":atlas-emacs-repl$ALIASES"
    fi
    mkdir -p "$STATEDIR"
    log="$STATEDIR/$NAME-nrepl.log"
    rm -f "$PROJECT/.nrepl-port"
    echo "Starting nREPL in $PROJECT (cider-nrepl $ver${ALIASES:+, aliases $ALIASES}); log: $log"
    (cd "$PROJECT" && nohup clojure \
       -Sdeps "{:deps {nrepl/nrepl {:mvn/version \"1.3.0\"} cider/cider-nrepl {:mvn/version \"$ver\"}}$sdeps_aliases}" \
       "-M${ALIASES}" -m nrepl.cmdline \
       --middleware '[cider.nrepl/cider-middleware]' --port 0 >"$log" 2>&1 &)
    # JVM start + first-time dependency download can take a while; report
    # progress (with the log's last line) so a slow start doesn't look frozen.
    start=$SECONDS; next=$((SECONDS + 10)); ready=""
    while (( SECONDS - start < 240 )); do
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
      echo "ERROR: nREPL not answering after 240s. Last log lines ($log):" >&2
      tail -n5 "$log" >&2
      exit 1
    fi
    newport="$(cat "$PROJECT/.nrepl-port")"
    echo "repl: port $newport (ready in $((SECONDS - start))s)"
    if [[ -n "$BOOT" ]]; then
      echo "boot: evaluating --boot form …"
      clj-nrepl-eval -p "$newport" --timeout 600000 "$BOOT" 2>&1 | grep -v '^\*=' | tail -3
    fi
    ;;
  ensure)
    # Keep the daemon's current REPL while it's alive: .nrepl-port can be
    # rewritten by any other tool that starts a REPL in the same directory.
    if [[ -z "$PORT" ]] && daemon_up; then
      cur="$( [[ "$(eval_text "(atlas-layout/llm-status)" 2>/dev/null)" =~ cider=localhost:([0-9]+) ]] && echo "${BASH_REMATCH[1]}" )"
      if [[ -n "$cur" ]] && port_alive "$cur"; then
        PORT="$cur"; echo "keeping the daemon's current REPL on port $PORT (pass --port to switch)"
      fi
    fi
    [[ -n "$PORT" ]] || resolve_port
    if ! has_cider_middleware "$PORT"; then
      echo "WARNING: the REPL on port $PORT has no CIDER middleware — views work, but CIDER features (completion, M-. into code, docs) won't. Start a full REPL with: $(basename "$SELF") repl --project $PROJECT" >&2
    fi
    rdir="$(repl_dir "$PORT")"
    if [[ -n "$rdir" && "$rdir" != "$PROJECT" ]]; then
      echo "WARNING: the REPL on port $PORT runs in $rdir, not $PROJECT — views will show that REPL's code and registry" >&2
    fi
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
    echo "status: $(full_status)"
    echo "attach: $SELF attach --project $PROJECT"
    ;;
  attach)
    conf="${XDG_CONFIG_HOME:-$HOME/.config}/atlas-emacs/env"
    if [[ -f "$conf" ]]; then
      while IFS='=' read -r key value; do
        key="${key//[[:space:]]/}"
        [[ "$key" =~ ^ATLAS_EMACS_[A-Z_]+$ ]] || continue
        value="${value%%#*}"                       # trailing comment
        value="${value#"${value%%[![:space:]]*}"}" # leading spaces
        value="${value%"${value##*[![:space:]]}"}" # trailing spaces
        [[ -n "${!key:-}" ]] || export "$key=$value"
      done < "$conf"
    fi
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
    if daemon_up; then full_status; else echo "socket=$SOCKET down"; fi
    ;;
  list)
    found=""
    for sock in "$SOCKDIR"/atlas-*; do
      [[ -S "$sock" ]] || continue
      found=1
      if timeout 5 "$CLIENT" --socket-name="$sock" --eval t >/dev/null 2>&1; then
        SOCKET="$sock"; echo "$(full_status)"
      else
        echo "socket=$sock (no daemon answering — stale socket; remove it with: rm $sock)"
      fi
    done
    [[ -n "$found" ]] || echo "no atlas daemons in $SOCKDIR"
    ;;
  stop)
    # --socket PATH stops a daemon by its socket (e.g. one shown by `list`).
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

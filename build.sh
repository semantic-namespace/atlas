#!/usr/bin/env bash

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

print_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Build and deploy atlas modules (core + dev-tools + ui)

OPTIONS:
    -h, --help          Show this help message
    -i, --install       Build and install to local Maven (~/.m2)
    -d, --deploy        Build and deploy to Clojars
    --all               Build all public modules (core, dev-tools, ui)
    --core              Include core module
    --bt                Include bt module (behaviour-tree ontology)
    --store             Include store module (registry snapshots)
    --cloud             Include cloud module (local only; separate private repo)
    --dev               Include dev-tools module
    --ui                Include ui module

    No module flags = core, dev-tools and ui (same as --all).
    bt, store and cloud are opt-in: --all does not build them.

EXAMPLES:
    $0                         # Build all public modules
    $0 --all                   # Same, explicit
    $0 --all --cloud           # All public modules + cloud (local)
    $0 --install               # Install all public modules to local Maven
    $0 --deploy                # Deploy all public modules to Clojars
    $0 --install --core        # Install core only
    $0 --install --core --bt   # Install core, then the bt module that needs it
    $0 --deploy --ui           # Deploy ui only

EOF
}

INSTALL=false
DEPLOY=false
BUILD_CORE=false
BUILD_BT=false
BUILD_STORE=false
BUILD_CLOUD=false
BUILD_DEV=false
BUILD_UI=false
HAS_MODULE_FLAG=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help) print_usage; exit 0 ;;
        -i|--install) INSTALL=true; shift ;;
        -d|--deploy) DEPLOY=true; shift ;;
        --all) BUILD_CORE=true; BUILD_DEV=true; BUILD_UI=true; HAS_MODULE_FLAG=true; shift ;;
        --core) BUILD_CORE=true; HAS_MODULE_FLAG=true; shift ;;
        --bt) BUILD_BT=true; HAS_MODULE_FLAG=true; shift ;;
        --store) BUILD_STORE=true; HAS_MODULE_FLAG=true; shift ;;
        --cloud) BUILD_CLOUD=true; HAS_MODULE_FLAG=true; shift ;;
        --dev) BUILD_DEV=true; HAS_MODULE_FLAG=true; shift ;;
        --ui) BUILD_UI=true; HAS_MODULE_FLAG=true; shift ;;
        *) echo -e "${RED}Unknown option: $1${NC}"; print_usage; exit 1 ;;
    esac
done

# No module flags = core, dev-tools, ui. bt, store and cloud stay opt-in:
# cloud is a separate private repo absent from most checkouts, and bt/store are
# leaf modules that a plain `build.sh` has never built — folding them in would
# change what an existing `--deploy` run publishes.
if [ "$HAS_MODULE_FLAG" = false ]; then
    BUILD_CORE=true; BUILD_DEV=true; BUILD_UI=true
fi

# Check credentials if deploying
if [ "$DEPLOY" = true ]; then
    if [ ! -f "$HOME/.lein/credentials.clj" ] && [ ! -f "$HOME/.lein/credentials.clj.gpg" ]; then
        if [ -z "$CLOJARS_USERNAME" ] || [ -z "$CLOJARS_PASSWORD" ]; then
            echo -e "${RED}ERROR: Clojars credentials not found${NC}"
            echo "Either:"
            echo "  1. Use ~/.lein/credentials.clj"
            echo "  2. Set CLOJARS_USERNAME and CLOJARS_PASSWORD"
            exit 1
        fi
    else
        echo -e "${GREEN}Using credentials from ~/.lein/credentials.clj${NC}"
    fi
fi

# bt and store have no build-and-deploy.sh of their own; their build.clj
# exposes clean/jar/install, so drive tools.build directly. $3 says whether the
# module has a :deploy alias — store does not, and a missing alias must not take
# down a whole --deploy run.
build_module() {
    local dir="$1" artifact="$2" has_deploy="$3"
    cd "$SCRIPT_DIR/$dir"
    clojure -T:build clean
    clojure -T:build jar
    if [ "$INSTALL" = true ]; then
        clojure -T:build install
    fi
    if [ "$DEPLOY" = true ]; then
        if [ "$has_deploy" = true ]; then
            local version
            version=$(tr -d '[:space:]' < "$SCRIPT_DIR/VERSION")
            clojure -X:deploy :artifact "\"target/${artifact}-${version}.jar\""
        else
            echo -e "${YELLOW}Skipping Clojars deploy for ${artifact}: no :deploy alias in ${dir}/deps.edn${NC}"
        fi
    fi
}

# Determine action flag""
if [ "$DEPLOY" = true ]; then
    ACTION="--deploy"
elif [ "$INSTALL" = true ]; then
    ACTION="--install"
fi

# Core (must build before cloud, which depends on it)
if [ "$BUILD_CORE" = true ]; then
    echo -e "${GREEN}━━━ atlas core ━━━${NC}"
    cd "$SCRIPT_DIR/core"
    ./build-and-deploy.sh $ACTION
    echo ""
fi

# Cloud (depends on core; separate private repo — skip if absent)
if [ "$BUILD_CLOUD" = true ]; then
    if [ ! -d "$SCRIPT_DIR/cloud" ]; then
        echo -e "${RED}Skipping atlas-cloud: cloud/ not present in this checkout${NC}"
    else
        echo -e "${GREEN}━━━ atlas-cloud ━━━${NC}"
        cd "$SCRIPT_DIR/cloud"
        ./build-and-deploy.sh $ACTION
        echo ""
    fi
fi

# bt (depends on core by :mvn/version, so core must be installed first)
if [ "$BUILD_BT" = true ]; then
    if [ ! -f "$SCRIPT_DIR/bt/deps.edn" ]; then
        echo -e "${RED}Skipping atlas-bt: bt/ not present in this checkout${NC}"
    else
        echo -e "${GREEN}━━━ atlas-bt ━━━${NC}"
        build_module bt atlas-bt true
        echo ""
    fi
fi

# store (deliberately depends on no atlas module, so order does not matter)
if [ "$BUILD_STORE" = true ]; then
    if [ ! -f "$SCRIPT_DIR/store/deps.edn" ]; then
        echo -e "${RED}Skipping atlas-store: store/ not present in this checkout${NC}"
    else
        echo -e "${GREEN}━━━ atlas-store ━━━${NC}"
        build_module store atlas-store false
        echo ""
    fi
fi

# Dev-tools
if [ "$BUILD_DEV" = true ]; then
    echo -e "${GREEN}━━━ atlas-dev ━━━${NC}"
    cd "$SCRIPT_DIR/dev-tools"
    ./build-and-deploy.sh $ACTION
    echo ""
fi

# UI
if [ "$BUILD_UI" = true ]; then
    echo -e "${GREEN}━━━ atlas-ui ━━━${NC}"
    cd "$SCRIPT_DIR/ui"
    ./build-and-deploy.sh $ACTION
    echo ""
fi

echo -e "${GREEN}All done!${NC}"

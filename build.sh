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
    --core              Include core module
    --dev               Include dev-tools module
    --ui                Include ui module

    No module flags = all modules.

EXAMPLES:
    $0                         # Build all
    $0 --install               # Install all to local Maven
    $0 --deploy                # Deploy all to Clojars
    $0 --install --core        # Install core only
    $0 --install --core --dev  # Install core + dev-tools
    $0 --deploy --ui           # Deploy ui only

EOF
}

INSTALL=false
DEPLOY=false
BUILD_CORE=false
BUILD_DEV=false
BUILD_UI=false
HAS_MODULE_FLAG=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help) print_usage; exit 0 ;;
        -i|--install) INSTALL=true; shift ;;
        -d|--deploy) DEPLOY=true; shift ;;
        --core) BUILD_CORE=true; HAS_MODULE_FLAG=true; shift ;;
        --dev) BUILD_DEV=true; HAS_MODULE_FLAG=true; shift ;;
        --ui) BUILD_UI=true; HAS_MODULE_FLAG=true; shift ;;
        *) echo -e "${RED}Unknown option: $1${NC}"; print_usage; exit 1 ;;
    esac
done

# No module flags = build all
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

# Determine action flag
ACTION=""
if [ "$DEPLOY" = true ]; then
    ACTION="--deploy"
elif [ "$INSTALL" = true ]; then
    ACTION="--install"
fi

# Core
if [ "$BUILD_CORE" = true ]; then
    echo -e "${GREEN}━━━ atlas core ━━━${NC}"
    cd "$SCRIPT_DIR/core"
    ./build-and-deploy.sh $ACTION
    echo ""
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

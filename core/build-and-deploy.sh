#!/usr/bin/env bash

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

print_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Build and deploy atlas core library

OPTIONS:
    -h, --help          Show this help message
    -i, --install       Build and install to local Maven (~/.m2)
    -d, --deploy        Build and deploy to Clojars (requires CLOJARS_USERNAME and CLOJARS_PASSWORD)
    -s, --skip-clean    Skip cleaning target directory

EXAMPLES:
    # Just build JAR
    $0

    # Build and install to local Maven
    $0 --install

    # Build and deploy to Clojars
    $0 --deploy

EOF
}

# Default options
INSTALL=false
DEPLOY=false
SKIP_CLEAN=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            print_usage
            exit 0
            ;;
        -i|--install)
            INSTALL=true
            shift
            ;;
        -d|--deploy)
            DEPLOY=true
            shift
            ;;
        -s|--skip-clean)
            SKIP_CLEAN=true
            shift
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            print_usage
            exit 1
            ;;
    esac
done

# Check for Clojars credentials if deploying
if [ "$DEPLOY" = true ]; then
    if [ ! -f "$HOME/.lein/credentials.clj" ] && [ ! -f "$HOME/.lein/credentials.clj.gpg" ]; then
        if [ -z "$CLOJARS_USERNAME" ] || [ -z "$CLOJARS_PASSWORD" ]; then
            echo -e "${RED}ERROR: Clojars credentials not found${NC}"
            echo "Either:"
            echo "  1. Use ~/.lein/credentials.clj (recommended)"
            echo "  2. Set CLOJARS_USERNAME and CLOJARS_PASSWORD environment variables"
            exit 1
        fi
    else
        echo -e "${GREEN}Using credentials from ~/.lein/credentials.clj${NC}"
    fi
fi

# Build JAR
echo -e "${GREEN}Building JAR...${NC}"

if [ "$SKIP_CLEAN" = false ]; then
    clojure -T:build clean
fi

clojure -T:build jar

echo -e "${GREEN}✓ JAR built successfully${NC}\n"

# Optional: Install to local Maven
if [ "$INSTALL" = true ]; then
    echo -e "${GREEN}Installing to local Maven repository...${NC}"
    clojure -T:build install
    echo -e "${GREEN}✓ Installed to ~/.m2/repository${NC}\n"
fi

# Optional: Deploy to Clojars
if [ "$DEPLOY" = true ]; then
    echo -e "${GREEN}Deploying to Clojars...${NC}"

    # Extract version from build.clj and find the JAR file
    VERSION=$(grep "def version" build.clj | sed -e 's/.*"\(.*\)".*/\1/')
    JAR_FILE="target/atlas-${VERSION}.jar"

    if [ ! -f "$JAR_FILE" ]; then
        echo -e "${RED}ERROR: JAR file not found: $JAR_FILE${NC}"
        exit 1
    fi

    clojure -X:deploy :artifact "\"$JAR_FILE\""
    echo -e "${GREEN}✓ Deployed to Clojars${NC}\n"
fi

echo -e "${GREEN}Done!${NC}"

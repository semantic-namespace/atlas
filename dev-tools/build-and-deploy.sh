#!/usr/bin/env bash

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

print_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Build and deploy atlas-dev library

OPTIONS:
    -h, --help          Show this help message
    -i, --install       Build and install to local Maven (~/.m2)
    -d, --deploy        Build and deploy to Clojars

EXAMPLES:
    $0                  # Just build JAR
    $0 --install        # Build and install to local Maven
    $0 --deploy         # Build and deploy to Clojars

EOF
}

INSTALL=false
DEPLOY=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help) print_usage; exit 0 ;;
        -i|--install) INSTALL=true; shift ;;
        -d|--deploy) DEPLOY=true; shift ;;
        *) echo -e "${RED}Unknown option: $1${NC}"; print_usage; exit 1 ;;
    esac
done

if [ "$DEPLOY" = true ]; then
    if [ ! -f "$HOME/.lein/credentials.clj" ] && [ ! -f "$HOME/.lein/credentials.clj.gpg" ]; then
        if [ -z "$CLOJARS_USERNAME" ] || [ -z "$CLOJARS_PASSWORD" ]; then
            echo -e "${RED}ERROR: Clojars credentials not found${NC}"
            exit 1
        fi
    else
        echo -e "${GREEN}Using credentials from ~/.lein/credentials.clj${NC}"
    fi
fi

echo -e "${GREEN}Building JAR...${NC}"
clojure -T:build jar
echo -e "${GREEN}✓ JAR built successfully${NC}\n"

if [ "$INSTALL" = true ]; then
    echo -e "${GREEN}Installing to local Maven repository...${NC}"
    clojure -T:build install
    echo -e "${GREEN}✓ Installed to ~/.m2/repository${NC}\n"
fi

if [ "$DEPLOY" = true ]; then
    echo -e "${GREEN}Deploying to Clojars...${NC}"
    VERSION=$(grep "def version" build.clj | sed -e 's/.*"\(.*\)".*/\1/')
    JAR_FILE="target/atlas-dev-${VERSION}.jar"
    if [ ! -f "$JAR_FILE" ]; then
        echo -e "${RED}ERROR: JAR file not found: $JAR_FILE${NC}"
        exit 1
    fi
    clojure -X:deploy :artifact "\"$JAR_FILE\""
    echo -e "${GREEN}✓ Deployed to Clojars${NC}\n"
fi

echo -e "${GREEN}Done!${NC}"

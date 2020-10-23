#!/usr/bin/env bash
set -euo pipefail

VSCODE_EXT_DIR="$HOME/.vscode/extensions"
PYLANCE_DIR_PATTERN='ms-python.vscode-pylance*'

PYLANCE_DIR=$(find $VSCODE_EXT_DIR -name "$PYLANCE_DIR_PATTERN" -print -quit)
node $PYLANCE_DIR/dist/server.bundle.js "$@"

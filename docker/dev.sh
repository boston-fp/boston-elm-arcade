#!/bin/bash

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Hack, can't figure out why this directory
# is getting corrupted when we mount it in
"$SCRIPT_DIR"/enter-container.sh \
    $("if [ -d elm-stuff ]; then rm -r elm-stuff fi && /bin/bash")
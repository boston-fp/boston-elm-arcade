#!/bin/bash

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

COMMAND="$@"

if [[ -z $COMMAND ]]; then
    COMMAND="/bin/bash"
fi

if [[ -z $DOCKER_ARGS ]]; then
    DOCKER_ARGS="--rm -it"
fi

if [[ -z $CONTAINER_TAG ]]; then
    CONTAINER_TAG="latest"
fi
echo "command: "
echo $COMMAND

docker run \
    --name "boston-elm-arcade" \
    -p 3000:3000 \
    -p 8080:8080 \
    -v "${SCRIPT_DIR}/../:/boston-elm-arcade" \
    -w "/boston-elm-arcade" \
    $DOCKER_ARGS \
    depot-ui-builder:$CONTAINER_TAG \
    $COMMAND

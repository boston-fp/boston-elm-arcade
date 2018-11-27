#!/bin/bash

set -e

if [[ -z $CONTAINER_TAG ]]; then
    CONTAINER_TAG="latest"
fi

echo "$CONTAINER_TAG"
docker build -t boston-elm-arcade:$CONTAINER_TAG .

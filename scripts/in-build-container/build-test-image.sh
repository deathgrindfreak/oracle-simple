#!/usr/bin/env sh

# This script builds docker images suitable for deployment. It is used in the
# CircleCI build to build the actual images used for deployment and also used
# used locally for building images used by the functional tests.
#
# It uses the buildpack tools for building these images, so it must be run
# inside a buildpack-based container.

set -ex

if [ $# -lt 2 ]; then
  echo "Usage: ./scripts/build-test-image.sh <image name> <image tag> <cache registry>"
  exit 1
fi;

NAME=$1
TAG=$2
CACHE_REGISTRY=$3

CACHE_FROM=""
if [ -n "$CACHE_REGISTRY" ]; then
  IMAGE_FOR_CACHE="$CACHE_REGISTRY/$NAME:latest"
  if docker manifest inspect "${IMAGE_FOR_CACHE}" > /dev/null; then
    docker pull "${IMAGE_FOR_CACHE}"
    CACHE_FROM="$IMAGE_FOR_CACHE"
  fi
fi

cp ./.stack-work/dist/functional-tests ./containers/oracle-buildpack/.

docker build \
  -t "${NAME}:${TAG}" \
  --build-arg ORACLE_VERSION="23.4.0.24.05" \
  -f ./containers/oracle-buildpack/Dockerfile \
  ${CACHE_FROM:+"--cache-from"} ${CACHE_FROM:+"$CACHE_FROM"} \
  ./containers/oracle-buildpack

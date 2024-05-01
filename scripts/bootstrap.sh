#! /bin/sh
set -ex

# The special .env file is used by docker to help with file redirection, especially for editor integration
echo "PROJECT_DIR=$PWD" > .env

if [ "$DOCKER_HOST" = "" ]; then
  echo "DOCKER_SOCKET_PATH=/var/run/docker.sock" >> .env
else
  socket_path=$(echo $DOCKER_HOST | sed s@^unix://@@)
  echo "DOCKER_SOCKET_PATH=$socket_path" >> .env
fi

if docker volume inspect stack -f "stack volume exists"; then
  echo "skipping volume creation"
else
  docker volume create stack
fi

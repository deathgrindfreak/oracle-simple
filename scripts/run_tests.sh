#!/usr/bin/env sh

shutdown () {
  docker-compose -f docker-compose.test.yml stop
}

trap shutdown EXIT

docker-compose -f docker-compose.test.yml up -d dev-db
docker-compose -f docker-compose.test.yml run --rm functional-tests

exit $?

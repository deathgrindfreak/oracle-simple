#!/bin/sh

./scripts/in-build-container.sh ./scripts/in-build-container/build.sh
./scripts/in-gke-tools-container.sh ./scripts/in-build-container/build-images.sh

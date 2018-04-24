#!/bin/bash

echo "executing sub-make: $@ ; in directory $PWD"

make "$@"

echo "finishing sub-make: $@ ; in directory $PWD"

#!/bin/bash

echo "Making a jar out of this Clojure project"

lein compile

lein uberjar

echo "Results are placed in ./target/ directory."

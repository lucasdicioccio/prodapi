#!/bin/bash
# A script to generate servant-docs and others.


mkdir -p gen/docs

cabal run -- prodapi-gen-exe | grep '^-' | sed 's/-//' | while read -r line ; do
  keyval=(${line})
  cabal run -- prodapi-gen-exe ${keyval[@]} > "gen/docs/${keyval[0]}-${keyval[1]}.md"
done


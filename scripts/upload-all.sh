#!/bin/bash

# set -x

projects=$(cat <<BOB
prodapi-core
prodapi-web
prodapi-userauth
prodapi-proxy
prodapi-pg
BOB
)

echo "${projects}" | while read -r line; do
  echo "> ${line}"

  dir=$(mktemp -d dist-docs.XXXXXX)
  trap 'rm -r "$dir"' EXIT

  cabal build  "${line}" 
  cabal sdist --builddir="$dir" "${line}"
  cabal upload --publish ${dir}/sdist/${line}-*.tar.gz

  # cabal haddock --builddir="$dir" --haddock-for-hackage --enable-doc "${line}" 
  # cabal upload -d --publish $dir/*-docs.tar.gz
  rm -r "$dir"
  unset dir
done

echo "done"

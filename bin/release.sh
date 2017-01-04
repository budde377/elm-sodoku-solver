#!/usr/bin/env bash

git checkout gh-pages
git reset --hard origin/master
elm-make src/Main.elm --output=index.html
git add index.html
git commit -m "Creating github page"
git push --force

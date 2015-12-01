rm -rf dist
mkdir dist
elm make src/Battleship.elm --output dist/index.html
cp -r src/img dist

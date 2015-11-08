echo 'watching elm file'
cd examples/elm && filewatcher main.elm 'elm-make main.elm --output ../public/js/main.js'
cd ../..

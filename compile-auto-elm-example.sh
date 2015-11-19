echo 'watching elm file'
cd examples/elm && filewatcher '**/*.elm' 'elm-make main.elm --output ../public/js/main.js && cp Html/widgets.elm ../../src/Html/widgets.elm'
cd ../..

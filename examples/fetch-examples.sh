cd "$(dirname $0)"
curl 'https://raw.githubusercontent.com/halohalospecial/atom-elmjutsu/5f912acfc9bff4ab055097e3bc2e1fb77f5d992b/elm/Indexer.elm' > elmjutsu-5k.elm
git clone 'https://github.com/rtfeldman/elm-spa-example.git'
cd elm-spa-example
git checkout 28d9288c7a67cf53bb628acfba79689bc5516509
elm-make --yes
cd ..

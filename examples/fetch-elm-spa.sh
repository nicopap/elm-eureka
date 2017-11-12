cd "$(dirname $0)"
git clone 'https://github.com/rtfeldman/elm-spa-example.git'
cd elm-spa-example
git checkout 28d9288c7a67cf53bb628acfba79689bc5516509
elm-make --yes
cd ..

#!/bin/bash

diff -Bw <(sed 's/--.*$//' examples/elmjutsu-5k.elm) <(cargo run --example lexdelex)

#!/bin/sh
exec erl -pa ebin deps/*/ebin -boot start_clean \
    -sname herlon +K true +A 10 \
    -s herlon \
    -nakaz priv/herlon.yaml \
    -setcookie herlon
#    -config priv/app \

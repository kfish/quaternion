#!/bin/sh

# Attempt elm-test a few times if the test-runner runs out of memory
elm-test
if [ $? -eq 134 ] ; then
    elm-test
    if [ $? -eq 134 ] ; then
        elm-test
    fi
fi

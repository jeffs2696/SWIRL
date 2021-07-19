#! /bin/bash

gnuplot -persist <<- blah
    load "plottingScripts/plotL2OfSoundSpeed.plt"
blah

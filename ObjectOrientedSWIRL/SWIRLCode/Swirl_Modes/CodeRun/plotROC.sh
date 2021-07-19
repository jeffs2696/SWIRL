#! /bin/bash

gnuplot -persist <<- blah
    load "plottingScripts/plotRateOfConvergence.plt"
blah

#! /bin/bash

gnuplot -persist <<- blah
    load "../plottingGNUconfig/plotRateOfConvergence.plt"
blah

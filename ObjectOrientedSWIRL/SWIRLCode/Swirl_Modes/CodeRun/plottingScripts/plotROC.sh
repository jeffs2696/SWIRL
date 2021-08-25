#! /bin/bash

gnuplot -persist <<- blah
    load "../plottingGNUconfig/plotRateOfConvergence.plt"
    load "../plottingGNUconfig/plotRateOfConvergenceSource.plt"
blah

#! /bin/bash

gnuplot -persist <<- blah
    load "../plottingGNUconfig/plotL2OfSoundSpeed.plt"
    load "../plottingGNUconfig/plotL2OfSourceTerm.plt"
blah

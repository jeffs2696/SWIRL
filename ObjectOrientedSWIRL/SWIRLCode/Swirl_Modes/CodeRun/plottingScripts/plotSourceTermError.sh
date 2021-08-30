#! /bin/bash

gnuplot -persist <<- blah
    load "../plottingGNUconfig/plotSourceTermError.plt"
blah

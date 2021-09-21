#! /bin/bash

gnuplot -persist <<- blah
    load "../plottingGNUconfig/plotSourceTermData.plt"
blah

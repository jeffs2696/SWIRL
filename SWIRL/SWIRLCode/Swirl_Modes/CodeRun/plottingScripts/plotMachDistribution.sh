#! /bin/bash

gnuplot -persist <<- blah
    load "../plottingGNUconfig/plotFlowData.plt"
blah

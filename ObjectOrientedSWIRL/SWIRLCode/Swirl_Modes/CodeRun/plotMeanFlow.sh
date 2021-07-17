#! /bin/bash

gnuplot -persist <<- blah
    load "plotMeanFlow.plt"
blah

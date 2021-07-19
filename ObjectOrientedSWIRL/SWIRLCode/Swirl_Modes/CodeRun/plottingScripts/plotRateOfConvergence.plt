#! /bin/bash 
set term tikz
set output 'ROC.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "Number of Gridpoints" 
set ylabel "Observed Order-of-Accuracy"
#set xrange [-10:35]
#set yrange [-50:50] set logscale x 10 set logscale y 10 
set grid
plot "RateOfConvergenceForIntegration.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "2nd-Order Trapezoidal Rule"

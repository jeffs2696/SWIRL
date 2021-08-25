#! /bin/bash 
set term tikz
set output 'ROC_Source.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "$\\Delta r$" 
set ylabel "Observed Order-of-Accuracy"
# set yrange [1:3]
# set logscale y
# set ytics (1,2,3)
# set format y "%6.3f"
set grid
plot "../RateOfConvergenceForSourceTerm.dat" every ::1 using 1:2 with linespoints linecolor rgb "#008080" title  "?"

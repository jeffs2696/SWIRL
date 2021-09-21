#! bin/bash 
set term tikz 
set output 'L2OfSourceTerm.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "Number of Grid Points" 
set ylabel "L2 Error Exponent for a Base 10" font ",12"
set format y "{%L}"
set logscale x 10
set logscale y 10 
set grid
plot "../L2OfSourceTerm.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "L2"

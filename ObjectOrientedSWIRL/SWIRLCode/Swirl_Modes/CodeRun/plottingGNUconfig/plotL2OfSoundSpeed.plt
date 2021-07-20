#! bin/bash 
set term tikz 
set output 'L2OfSoundSpeed.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "Number of Grid Points" 
set ylabel "L2 Error" font ",12"
set logscale x 10
set logscale y 10 
set grid
plot "../L2OfSoundSpeed.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "L2"

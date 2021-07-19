#! bin/bash 
set term tikz 
set output 'L2OfSoundSpeed.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "Number of Gridpoints" 
set ylabel "L2 Error"
#set xrange [-10:35]
#set yrange [-50:50]
set logscale x 10
set logscale y 10 
set grid
plot "L2OfSoundSpeed.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "L2"

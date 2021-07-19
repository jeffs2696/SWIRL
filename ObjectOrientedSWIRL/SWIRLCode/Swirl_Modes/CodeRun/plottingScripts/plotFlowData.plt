#! /bin/bash 
set term tikz
set output 'MachDistribution.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "Radius [-]"
set ylabel "Flow Components"
#set xrange [-10:35]
#set yrange [-50:50] set logscale x 10 set logscale y 10 
set grid
plot "MeanFlowData513.dat" using 1:2 with linespoints linecolor rgb "#2A84A5" title  "$M_x$", \
     "MeanFlowData513.dat" using 1:3 with linespoints linecolor rgb "#CA472F" title  "$M_{\\theta }$", \
     "MeanFlowData513.dat" using 1:(sqrt($2**2+$3**2)) with linespoints linecolor rgb "#CA472F" title  "$M_{\\theta }$"

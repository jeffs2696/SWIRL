#! /bin/bash 
set term tikz
set output 'MachDistribution.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "Radius [-]"
set ylabel "Flow Components"
set samples 100
set grid
plot "../MeanFlowData/MeanFlowData513.dat" using 1:2 with linespoints pointinterval 10 linecolor rgb "#2A84A5" title  "$M_x$", \
     "../MeanFlowData/MeanFlowData513.dat" using 1:3 with linespoints pointinterval 10 linecolor rgb "#CA472F" title  "$M_{\\theta }$", \
     "../MeanFlowData/MeanFlowData513.dat" using 1:(sqrt($2**2+$3**2)) with linespoints pointinterval 10 linecolor rgb "#F6C85F" title  "$M_{total}$"

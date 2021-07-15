#! bin/bash 
set termopt enhanced    # turn on enhanced text mode
set multiplot layout 2, 1 
set xlabel "Radius" 
set ylabel ""
set key outside
#set xrange [-10:35]
#set yrange [-50:50]
set grid
plot "FlowDataInput3.dat" using 1:2 with linespoints

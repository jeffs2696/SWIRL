#! bin/bash 
set termopt enhanced    # turn on enhanced text mode
set xlabel "{/Symbol D}r" 
set ylabel ""
set logscale x 10
set logscale x 10
#set xrange [-10:35]
#set yrange [-50:50]
set grid
plot "SndSpeedRateOfConvergence.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "{/Symbol a}"

#lt 5 ps 1 lw 1 lc rgb "#008080" linespoints ls 1
#    "flowData.dat" using 1:4 with linespoints linecolor rgb "#003110" title  "~{/Symbol A}_{in}" ,\

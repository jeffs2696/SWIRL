#! bin/bash 

set xlabel "dr" 
set ylabel "epsilon"
#set xrange [-10:35]
#set yrange [-50:50]
set grid
plot "errorData.dat" using 1:2 with linespoints linecolor rgb "#008080"
#lt 5 ps 1 lw 1 lc rgb "#008080" linespoints ls 1

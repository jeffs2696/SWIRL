#! bin/bash 
set termopt enhanced    # turn on enhanced text mode
set xlabel "Radius" 
set ylabel "Mach Number"
#set xrange [-10:35]
#set yrange [-50:50]
set grid
plot "machOut.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "M_x", \
     "machOut.dat" using 1:4 with linespoints linecolor rgb "#006580" title  "M_{/Symbol q}"
#lt 5 ps 1 lw 1 lc rgb "#008080" linespoints ls 1

#! bin/bash 
set termopt enhanced    # turn on enhanced text mode
set xlabel "Radius" 
set ylabel "{/Symbol e}"
#set xrange [-10:35]
#set yrange [-50:50]
set grid
plot "errorData.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "{/Symbol e_{1}}", \
     "errorData.dat" using 1:3 with linespoints linecolor rgb "#006580" title  "{/Symbol e_{2}}", \
     "errorData.dat" using 1:4 with linespoints linecolor rgb "#006580" title  "{/Symbol e_{3}}", \
     "errorData.dat" using 1:5 with linespoints linecolor rgb "#006580" title  "{/Symbol e_{4}}"
#lt 5 ps 1 lw 1 lc rgb "#008080" linespoints ls 1

#! bin/bash 
set termopt enhanced    # turn on enhanced text mode
set xlabel "Radius" 
set ylabel "Source Term, S"
#set xrange [-10:35]
#set yrange [-50:50]
set grid
plot "CalcSourceData.dat" using 1:2 with linespoints linecolor rgb "#008080" title  " S_{1,MMS}", \
     "CalcSourceData.dat" using 1:3 with linespoints linecolor rgb "#006580" title  " S_{2,MMS}", \
     "CalcSourceData.dat" using 1:4 with linespoints linecolor rgb "#006580" title  " S_{3,MMS}", \
     "CalcSourceData.dat" using 1:5 with linespoints linecolor rgb "#006580" title  " S_{4,MMS}"
#lt 5 Calcps 1 lw 1 lc rgb "#008080" linespoints ls 1

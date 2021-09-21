#! bin/bash 
set term tikz
set output 'L2OfSoundSpeed.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "Number Of Grid Points" 
set ylabel "L2 Error"
#set xrange [-10:35]
#set yrange [-50:50]
set logscale x 10
set logscale y 10 
set grid
plot "L2OfSoundSpeed.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "L_2"

#lt 5 ps 1 lw 1 lc rgb "#008080" linespoints ls 1
#    "flowData.dat" using 1:4 with linespoints linecolor rgb "#003110" title  "~{/Symbol A}_{in}" ,\

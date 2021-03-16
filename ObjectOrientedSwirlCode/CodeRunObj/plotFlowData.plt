#! bin/bash 
set termopt enhanced    # turn on enhanced text mode
set xlabel "Radius" 
set ylabel ""
#set xrange [-10:35]
#set yrange [-50:50]
set grid
plot "flowData.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "M_{x,in}", \
     "flowData.dat" using 1:3 with linespoints linecolor rgb "#006580" title  "M_{/Symbol q,in}" ,\
     "flowData.dat" using 1:4 with linespoints linecolor rgb "#003110" title  "A_{in}" ,\
     "machOut.dat" using 1:2 with linespoints linecolor rgb "#001580" title  "M_{x,out}" ,\
     "machOut.dat" using 1:3 with linespoints linecolor rgb "#002080" title  "M_{/Symbol q,out}" ,\
     "machOut.dat" using 1:6 with linespoints linecolor rgb "#009910" title  "A_{out}" 
#lt 5 ps 1 lw 1 lc rgb "#008080" linespoints ls 1
#    "flowData.dat" using 1:4 with linespoints linecolor rgb "#003110" title  "~{/Symbol A}_{in}" ,\

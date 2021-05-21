#! bin/bash 
set termopt enhanced    # turn on enhanced text mode
set multiplot layout 2, 1 
set xlabel "Radius" 
set ylabel ""
set key outside
#set xrange [-10:35]
#set yrange [-50:50]
set grid
plot \
    "FlowDataInput9.dat" using 1:2 with linespoints linecolor rgb "#008080" title  "M_{x,in}", \
    "FlowDataOutput9.dat" using 1:2 with linespoints linecolor rgb "#001580" title  "M_{x,out}" 
plot \
    "FlowDataInput9.dat" using 1:3 with linespoints linecolor rgb "#006580" title  "M_{{/Symbol q},in}" ,\
     "FlowDataOutput9.dat" using 1:3 with linespoints linecolor rgb "#002080" title  "M_{{/Symbol q},out}" 
unset multiplot
#lt 5 ps 1 lw 1 lc rgb "#008080" linespoints ls 1
#    "FlowDataInput.dat" using 1:4 with linespoints linecolor rgb "#003110" title  "~{/Symbol A}_{in}" ,\

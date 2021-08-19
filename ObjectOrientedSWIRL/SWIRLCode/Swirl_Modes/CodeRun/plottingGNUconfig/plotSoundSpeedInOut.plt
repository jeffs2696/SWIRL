#! bin/bash 
set term tikz
set output "SoundSpeedFromIntegration.tex"
set termopt enhanced    # turn on enhanced text mode
# set multiplot layout 2, 1 
set xlabel "Radius" 
set ylabel "Speed Of Sound"
set key outside
set samples 100
# set yrange [-5.0e-11:5.0e-11]
set grid
plot for [i=7:7] \
    "../MeanFlowData/MeanFlowData".(1+2**i).".dat" using 1:5 with linespoints pointinterval 10 linecolor rgb "#2A84A5" title  "".(1+2**i)." Grid Points" , \
    "../MeanFlowData/MeanFlowData".(1+2**5).".dat" using 1:5 with linespoints pointinterval 10 linecolor rgb "#D8B655" title  "".(1+2**5)." Grid Points" , \
    "../MeanFlowData/MeanFlowData".(1+2**1).".dat" using 1:5 with linespoints pointtype 7 linecolor rgb "#CA472F" title  "".(1+2**1)." Grid Points" 
    # 'MeanFlowData'.(1+2**i).'.dat' using 1:4 with linespoints linecolor rgb "#008080" title  "A_{in}" 
# plot \
#     "FlowDataInput.dat" using 1:3 with linespoints linecolor rgb "#006580" title  "M_{{/Symbol q},in}" ,\
#      "FlowDataOutput.dat" using 1:3 with linespoints linecolor rgb "#002080" title  "M_{{/Symbol q},out}" 
# unset multiplot
#lt 5 ps 1 lw 1 lc rgb "#008080" linespoints ls 1
#    "FlowDataInput.dat" using 1:4 with linespoints linecolor rgb "#003110" title  "~{/Symbol A}_{in}" ,\

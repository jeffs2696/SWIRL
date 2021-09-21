#! /bin/bash 
set term tikz
set output 'SourceTermData.tex'
set termopt enhanced    # turn on enhanced text mode
# set xlabel "Radius*4 [-]"
# set ylabel "Source Term Error"
set samples 100
set grid
set key noautotitle


# set tmargin 0
# set bmargin 0
# set lmargin 1
# set rmargin 1

set multiplot layout 4,1 \
    columnsfirst scale 1.10,0.70 \
    margins 0.0, 0.90, 0.1, 0.98 \
    spacing 0.080 , 0.080
set size 1,0.55
plot "../SourceTermData1.dat" using 1:2 with linespoints pointinterval 10 linecolor rgb "#008080" title "S1", \
     "../SourceTermData1.dat" using 1:3 with linespoints pointinterval 10 linecolor rgb "#001080" title "S1"
set size 1,0.55
plot "../SourceTermData2.dat" using 1:2 with linespoints pointinterval 10 linecolor rgb "#008080" , \
     "../SourceTermData2.dat" using 1:3 with linespoints pointinterval 10 linecolor rgb "#001080" 
set size 1,0.55
plot "../SourceTermData3.dat" using 1:2 with linespoints pointinterval 10 linecolor rgb "#008080" , \
     "../SourceTermData3.dat" using 1:3 with linespoints pointinterval 10 linecolor rgb "#001080" 
set size 1,0.55
plot "../SourceTermData4.dat" using 1:2 with linespoints pointinterval 10 linecolor rgb "#008080" , \
     "../SourceTermData4.dat" using 1:3 with linespoints pointinterval 10 linecolor rgb "#001080" 
unset multiplot
    # "../SourceTermError.dat" using 1:4 with linespoints linecolor rgb "#004080" 

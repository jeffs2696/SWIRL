#! /bin/bash 
set term tikz
set output 'SourceTermError.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "Radius*4 [-]"
set ylabel "Source Term Error"
set samples 100
set grid
plot "../SourceTermError.dat" using 1:2 with linespoints linecolor rgb "#008080" , \
     "../SourceTermError.dat" using 1:3 with linespoints linecolor rgb "#001080" 
     # "../SourceTermError.dat" using 1:4 with linespoints linecolor rgb "#004080" 

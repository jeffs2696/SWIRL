#! /bin/bash 
set term tikz
set output 'SoundSpeedError.tex'
set termopt enhanced    # turn on enhanced text mode
set xlabel "Radius [-]"
set ylabel "Sound Speed Error"
set samples 100
set grid
plot "../SoundSpeedError.dat" using 1:2 with linespoints pointinterval 10 linecolor rgb "#2A84A5" 

#! bin/bash 

set xlabel "Real(k_x)"
set ylabel "Imaginary(k_x)"
#set xrange [-10:35]
#set yrange [-50:50]
set grid
plot "egvre.01" lt 7 ps 1 lw 1 lc rgb "#008080"

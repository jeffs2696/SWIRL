#! bin/bash 
set lmargin at screen 0.1
set rmargin at screen 0.9
set bmargin at screen 0.1
set tmargin at screen 0.9
set xlabel "radius" 
set ylabel "pbar"
set xrange [0:1]
set yrange [-1.25:1.25]
set grid
set multiplot
do for [step=1:12] {
    outfile = sprintf('egvre.%03.0F',step)
    print outfile
    plot outfile using 1:5 with linespoints lt step 
    if (step == 1) {
        unset border
        unset xtics
        unset ytics
        unset xlabel
        unset ylabel
    }
}
# do for [i=1:12] {
#     set key title sprintf(outfile)
# }
unset multiplot
#lt 5 ps 1 lw 1 lc rgb "#008080" linespoints ls 1

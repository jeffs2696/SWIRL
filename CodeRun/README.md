Note that the plotReport is linked to the Documentation/meanFlowMMS/ directory
Check the plottingGNUScripts to adjust the data ranges that is in the plotReport

## Naming Convention

The output files are placed in various directories,

1. 01-mean-flow/
2. 02-method-of-manufactured-solutions
3. 03-EVanalysis/
4. 04-plotReport


### 
EVanalysis naming convention

testname_npts_fd_domain_a/c_rmn_up/down_h/s_inner_h/s_outter

testname - unique identifier for that test case 
npts - number of grid points
fd - finite difference method, 1 - 2ndOrderCentral 2 - 4thOrderCentral
domain_a/c - annulus or cylinder
rmn - radial mode number 
up/down- upstream or downstream mode
h/s_inner/outer - hard or soft wall liner

MMS 

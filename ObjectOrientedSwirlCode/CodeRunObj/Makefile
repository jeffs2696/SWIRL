##################################################################
#
#   Makefile for SWIRL
#   Version 3.2
#
#   Kenneth A. Kousen
#   United Technologies Research Center
#   411 Silver Lane, MS 129-20
#   East Hartford, CT  06118
#   (860) 727-7743
#   kousen@utrc.utc.com
#   March 25, 1996
#
# modified to add the F2008 version of SWIRL
#
#   Ray Hixon
#   U. Toledo
#   October 19, 2016
#
#   Ray Hixon update, using LAPACK3.8.0 routines
#   March 8, 2018
#   
##  Jeff Severino 
###################################################################
F77      = gfortran
F90      = gfortran
F77FLAGS  = -g -c -Og -fbounds-check -fcheck-array-temporaries -Wconversion -Wsurprising -Wall
F90FLAGS  = -g -c -Og -std=f2008 -fbounds-check -fcheck-array-temporaries \
			-Wconversion -Wsurprising -Wall -ffpe-trap=invalid
#,overflow,underflow
#-Werror
#F90FLAGS   = -c -g -C -qextchk -qflttrap -qhsflt -qxflag=dd24 \
#	-v  -bloadmap:load.map
FEXT     = f
F90EXT   = f90
OEXT     = o

MMS_LOCATION = meanFlowMMS/
OUTPUT_DATA  =  \
			   output.data mach.dat  gam.non.acc gam.nonconv machOut.dat \
			   flowData.dat cv.waves.dat  

			  

all:
	(cd $(MMS_LOCATION); make); \
		(./plotSndSpdAlpha.sh);

clean: 
	rm $(OUTPUT_DATA)


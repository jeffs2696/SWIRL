
# Makefile to build and maintain the V070 System library
# Uses ar rsv nasalib.a (members that were updated) to update lib
#
# To execute: type make -f Makefile.iris
# This will recompile (optimized) the fortran files that have changed
# since nasalib_iris.a was made.

.SUFFIXES: .f .o

LIBRARY  = v070_nasalib_iris.a
all debug: $(LIBRARY)


# Define Macros

FC    = gfortran
FOPT  = -fcheck=bounds -O


# Define Library Contents
# v070 utilities
 
MEMBERS  = anfu.o    \
     	   anrt.o    \
   	       besj.o   \
	       besy.o   \
	       casc.o   \
	       eigen.o  \
	       isig.o   \
	       kernel.o \
	       phithh.o \
	       phixht.o \
	       rmode.o         
 

# Define Library Dependencies

.PRECIOUS  : $(LIBRARY)
$(LIBRARY) : $(MEMBERS) 
	ar rsv $@ $?
##	echo $%
#	ranlib $@
clean: 
	rm *.o

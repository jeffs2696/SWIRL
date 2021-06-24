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
###################################################################
F77      = f77
FFLAGS   = -c -O
#FFLAGS   = -c -g -C -qextchk -qflttrap -qhsflt -qxflag=dd24 \
#	-v  -bloadmap:load.map
FEXT     = f
OEXT     = o
OBJECTS  =     \
	swirl.o derivs.o global.o grid.o input.o analysis.o \
	boundary.o output.o rmach.o smach.o machout.o \
	sndspd.o fdgrid.o fdrivs.o kapsub.o interp.o indexx.o

all:  $(OBJECTS)
#
#  clean
clean:
	rm -f *.o *.c *.a
#                                                 
#   Define suffixes used in transformation rules. 
#                                                 
.SUFFIXES: .$(OEXT) .$(FEXT) .prj
#                                                               
#   Define transformation rule for Fortran to object files.     
#                                                               
.$(FEXT).$(OEXT):     ; $(F77) $(FFLAGS) $< 
#
#   Transformation rule from fortran to project files
.f.prj:
	ftnchek -project -noextern -library $<
#
#   Set up macro PRJS containing project filenames
PRJS= $(OBJECTS:.o=.prj)
#
check: $(PRJS)
	ftnchek $(PRJS)
#
#   Define dependency information for object files
#                                                               
swirl.o: swirl.f \
  pmnmax
analysis.o: analysis.f \
  pmnmax
boundary.o: boundary.f \
  pmnmax
derivs.o: derivs.f \
  pmnmax
global.o: global.f \
  pmnmax
grid.o:  grid.f \
  pmnmax
input.o: input.f
fdgrid.o: fdgrid.f \
  pmnmax
fdrivs.o: fdrivs.f \
  pmnmax
indexx.o: indexx.f
interp.o: interp.f \
  pmnmax
kapsub.o: kapsub.f
machout.o: machout.f \
  pmnmax
output.o: output.f \
  pmnmax
rmach.o: rmach.f \
  pmnmax
smach.o: smach.f \
  pmnmax
sndspd.o: sndspd.f \
  pmnmax

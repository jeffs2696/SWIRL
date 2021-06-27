##################################################################}}}}}}}
#
#   Makefile for SWIRL
#   Version 3.2
#
#   Kenneth A. Kousen #   United Technologies Research Center
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
#   Jeff Severino
#
#   -Added colored output and log message for errors 
#     see the all target
#   - add gdb flags
#   - added a line to the all: rule to run the executable in the CodeRun
#   folder 
#   - added floating point exception flags
#   - added -Wextra which caught some more bugs
#   TO DO:
#   		
###################################################################

# Defining colored output for ANSI escape sequences 
#


COM_COLOR   = \033[0;34m
OBJ_COLOR   = \033[0;36m
OK_COLOR    = \033[0;32m
ERROR_COLOR = \033[0;31m
WARN_COLOR  = \033[0;33m
NO_COLOR    = \033[m

OK_STRING = "[OK]"
ERROR_STRING = "[ERROR]"
WARN_STRING = "[WARNING]"
COM_STRING = "Compiling..."

F77      = gfortran
F90      = gfortran
F77FLAGS  = -g -c \
			-O0 \
			-std=f2008 \
			-fbacktrace -fcheck=all -fbounds-check -fcheck-array-temporaries \
			-ffpe-trap=denormal,invalid,zero,overflow,underflow \
			-Wall -Wline-truncation -Wuninitialized -Wconversion -Wsurprising \
		   	-Warray-temporaries -Wconversion-extra -Wextra -Werror \
			-fdump-fortran-global 

#-O -fbounds-check -fcheck-array-temporaries -Wconversion -Wsurprising -Wall
F90FLAGS  = -g -c \
			-O0 \
			-std=f2008 \
			-fbacktrace -fcheck=all -fbounds-check -fcheck-array-temporaries \
			-ffpe-trap=denormal,invalid,zero,overflow,underflow \
			-fdump-fortran-global \
			-fimplicit-none \
			-Wall -Wline-truncation -Wcharacter-truncation -Wuninitialized  \
			-Wconversion -Wsurprising -Warray-temporaries -Wconversion-extra -Wextra -Werror \
			-Wno-error=compare-reals \
			
#-Wno-error=do-subscript \ -Wno-error=array-temporaries \ -Wno-error=conversion-extra 
# -Wno-error=do-subscript
#F90FLAGS   = -c -g -C -qextchk -qflttrap -qhsflt -qxflag=dd24 \
#	-v  -bloadmap:load.map
#
FEXT     = f
F90EXT   = f90
OEXT     = o
OBJECTS  =                  \
    main.o					\
	swirlClassObject.o 				\
	L2NormModule.o			\
	derivsModule.o 			\
	globalModule.o 			\
	gridModule.o   			\
	analysisModule.o        \
	boundaryModule.o 		\
	indexxModule.o 			\
	outputModule.o 			\
	rmachModule.o 			\
	smachAndSndspdModule.o  \
    machoutModule.o 		\
	fdgridModule.o 			\
   	fdrivsModule.o 			\
	kapsubModule.o          \
    interpModule.o 			\
	Akima1D.f90             \
	akima433.o

EXECUTABLE_LOCATION = ../../CodeRun

EXE = swirlF2020

LIB_FILES = ../../LibraryFiles
MOD_FILES = ../../ModuleFiles
OBJ_FILES = ../../ObjectFiles
EXE_FILES = $(EXECUTABLE_LOCATION)
SRC_FILES = ../srcF2020

BLAS_LOCATION = $(LIB_FILES)/blas3.8.0
BLAS_OBJS =      \
         $(BLAS_LOCATION)/daxpy.o  \
         $(BLAS_LOCATION)/dcabs1.o \
         $(BLAS_LOCATION)/ddot.o   \
         $(BLAS_LOCATION)/dgemv.o  \
         $(BLAS_LOCATION)/dscal.o  \
         $(BLAS_LOCATION)/dznrm2.o \
         $(BLAS_LOCATION)/izamax.o \
         $(BLAS_LOCATION)/lsame.o  \
         $(BLAS_LOCATION)/zcopy.o  \
         $(BLAS_LOCATION)/zdscal.o \
         $(BLAS_LOCATION)/zgemm.o  \
         $(BLAS_LOCATION)/zgemv.o  \
         $(BLAS_LOCATION)/zgerc.o  \
         $(BLAS_LOCATION)/zscal.o  \
         $(BLAS_LOCATION)/zswap.o  \
         $(BLAS_LOCATION)/ztrmm.o  \
         $(BLAS_LOCATION)/ztrmv.o

FITPACK_LOCATION = $(LIB_FILES)/fitpack

FITPACK_OBJS =                        \
         $(FITPACK_LOCATION)/ceez.o   \
         $(FITPACK_LOCATION)/curv1.o  \
         $(FITPACK_LOCATION)/curv2.o  \
         $(FITPACK_LOCATION)/intrvl.o \
         $(FITPACK_LOCATION)/snhcsh.o \
         $(FITPACK_LOCATION)/terms.o

LAPACK_LOCATION = $(LIB_FILES)/lapack3.8.0

LAPACK_OBJS =           \
         $(LAPACK_LOCATION)/zggev.o        \
         $(LAPACK_LOCATION)/f90_zggev.o        \
         $(LAPACK_LOCATION)/disnan.o       \
         $(LAPACK_LOCATION)/dlabad.o       \
         $(LAPACK_LOCATION)/dladiv.o       \
         $(LAPACK_LOCATION)/dlaisnan.o     \
         $(LAPACK_LOCATION)/dlamch.o       \
         $(LAPACK_LOCATION)/dlapy2.o       \
         $(LAPACK_LOCATION)/dlapy3.o       \
         $(LAPACK_LOCATION)/ieeeck.o       \
         $(LAPACK_LOCATION)/ilaenv.o       \
         $(LAPACK_LOCATION)/ilazlc.o       \
         $(LAPACK_LOCATION)/ilazlr.o       \
         $(LAPACK_LOCATION)/iparam2stage.o \
         $(LAPACK_LOCATION)/iparmq.o       \
         $(LAPACK_LOCATION)/xerbla.o       \
         $(LAPACK_LOCATION)/zgeqr2.o       \
         $(LAPACK_LOCATION)/zgeqrf.o       \
         $(LAPACK_LOCATION)/zggbak.o       \
         $(LAPACK_LOCATION)/zggbal.o       \
         $(LAPACK_LOCATION)/zgghrd.o       \
         $(LAPACK_LOCATION)/zhgeqz.o       \
         $(LAPACK_LOCATION)/zlacgv.o       \
         $(LAPACK_LOCATION)/zlacpy.o       \
         $(LAPACK_LOCATION)/zladiv.o       \
         $(LAPACK_LOCATION)/zlange.o       \
         $(LAPACK_LOCATION)/zlanhs.o       \
         $(LAPACK_LOCATION)/zlarf.o        \
         $(LAPACK_LOCATION)/zlarfb.o       \
         $(LAPACK_LOCATION)/zlarfg.o       \
         $(LAPACK_LOCATION)/zlarft.o       \
         $(LAPACK_LOCATION)/zlartg.o       \
         $(LAPACK_LOCATION)/zlascl.o       \
         $(LAPACK_LOCATION)/zlaset.o       \
         $(LAPACK_LOCATION)/zlassq.o       \
         $(LAPACK_LOCATION)/zrot.o         \
         $(LAPACK_LOCATION)/ztgevc.o       \
         $(LAPACK_LOCATION)/zung2r.o       \
         $(LAPACK_LOCATION)/zungqr.o       \
         $(LAPACK_LOCATION)/zunm2r.o       \
         $(LAPACK_LOCATION)/zunmqr.o

all:  $(OBJECTS)
	@printf "%b" "$(COM_COLOR)$(COM_COLOR) $(OBJ_COLOR)$(@)$(NO_COLOR)\n" "Compiling $@\n";
	@$(F90) -o $(EXE) $(BLAS_OBJS) \
             $(LAPACK_OBJS) $(FITPACK_OBJS) $(OBJECTS) 2> $@.log; \
	RESULT=$$?; \
		if [ $$RESULT -ne 0 ]; then \
			printf "%-60b%b" "$(COM_COLOR)$(COM_STRING)$(OBJ_COLOR) $@" "$(ERROR_COLOR)$(ERROR_STRING)$(NO_COLOR)\n"; \
		elif [ -s $@.log ]; then \
			printf "%-60b%b" "$(COM_COLOR)$(COM_STRING)$(OBJ_COLOR) $@" "$(WARN_COLOR)$(WARN_STRING)$(NO_COLOR)\n"; \
			else  \
			printf "%-60b%b" "$(COM_COLOR)$(COM_STRING)$(OBJ_COLOR) $(@F)" "$(OK_COLOR)$(OK_STRING)$(NO_COLOR)\n"; \
			fi; \
			cat $@.log; \
			rm -f $@.log; \
			exit $$RESULT

	@mv $(EXE) $(EXECUTABLE_LOCATION)
	@(cd $(EXECUTABLE_LOCATION); ./$(EXE))
#
#  clean
clean:
	rm -f *.o *.c *.a *.mod

cleanAll:
	rm -f *.o *.c *.a *.mod;                         \
       (cd $(FITPACK_LOCATION); rm -f *.o *.c *.a *.mod); \
       (cd $(LAPACK_LOCATION); rm -f *.o *.c *.a *.mod); \
       (cd $(BLAS_LOCATION); rm -f *.o *.c *.a *.mod)
#                                                 
#   Define suffixes used in transformation rules. 
#                                                 
# .SUFFIXES: .$(OEXT) .$(FEXT) .prj 
#                                                               
#   Define transformation rule for Fortran to object files.     
#                                                               
# .$(FEXT).$(OEXT):     ; $(F90) $(F90FLAGS) $< 
#
#
#   Transformation rule from fortran to project files
# .f.prj:
# 	ftnchek -project -noextern -library $<
#
#   Set up macro PRJS containing project filenames
# PRJS= $(OBJECTS:.o=.prj)
#
# check: $(PRJS)
# 	ftnchek $(PRJS)
#
#   Define dependency information for object files
#                                                               
main.o: main.f90        \
         analysisModule.o \
         boundaryModule.o \
         derivsModule.o   \
         fdgridModule.o   \
         fdrivsModule.o   \
         globalModule.o   \
         gridModule.o     \
         interpModule.o   \
	 machoutModule.o  \
         outputModule.o   \
	 rmachModule.o    \
	 smachAndSndspdModule.o \
	 L2NormModule.o         \
	 swirlClassObject.o 
	$(F90) $(F90FLAGS) main.f90

akima433.o: akima433.f
	$(F77) $(F77FLAGS) akima433.f

Akima1D.o:  Akima1D.f90 \
	    akima433.o
	$(F90) $(F90FLAGS) Akima1D.f90

analysisModule.o:  analysisModule.f90 \
                  $(LAPACK_LOCATION)/zggev.o\
				  $(LAPACK_LOCATION)/f90_zggev.o 
	$(F90) $(F90FLAGS) -I$(LAPACK_LOCATION) analysisModule.f90

boundaryModule.o:  boundaryModule.f90
	$(F90) $(F90FLAGS) boundaryModule.f90

derivsModule.o:  derivsModule.f90
	$(F90) $(F90FLAGS) derivsModule.f90

globalModule.o:  globalModule.f90
	$(F90) $(F90FLAGS) globalModule.f90

gridModule.o:  gridModule.f90
	$(F90) $(F90FLAGS) gridModule.f90

fdgridModule.o:  fdgridModule.f90
	$(F90) $(F90FLAGS) fdgridModule.f90

fdrivsModule.o:  fdrivsModule.f90
	$(F90) $(F90FLAGS) fdrivsModule.f90

indexxModule.o:  indexxModule.f90
	$(F90) $(F90FLAGS) indexxModule.f90

interpModule.o:  interpModule.f90 \
         derivsModule.o   \
         fdgridModule.o   \
         fdrivsModule.o   \
         gridModule.o     \
         $(BLAS_LOCATION)/dgemv.o \
         $(FITPACK_LOCATION)/curv1.o \
         $(FITPACK_LOCATION)/curv2.o
	$(F90) $(F90FLAGS) interpModule.f90

kapsubModule.o:  kapsubModule.f90 \
                  $(LAPACK_LOCATION)/zggev.o
	$(F90) $(F90FLAGS) -I$(LAPACK_LOCATION) kapsubModule.f90

machoutModule.o:  machoutModule.f90
	$(F90) $(F90FLAGS) machoutModule.f90

outputModule.o:  outputModule.f90 \
                 indexxModule.o   \
                 kapsubModule.o
	$(F90) $(F90FLAGS) outputModule.f90

rmachModule.o:  rmachModule.f90 \
	        Akima1D.o
	$(F90) $(F90FLAGS) rmachModule.f90

smachModule.o:  smachModule.f90 \
	        Akima1D.o
	$(F90) $(F90FLAGS) smachModule.f90

smachAndSndspdModule.o:  smachAndSndspdModule.f90 \
	        Akima1D.o
	$(F90) $(F90FLAGS) smachAndSndspdModule.f90

sndspdModule.o:  sndspdModule.f90
	$(F90) $(F90FLAGS) sndspdModule.f90

L2NormModule.o: L2NormModule.f90
	$(F90) $(F90FLAGS) L2NormModule.f90 

swirlClassObject.o: swirlClassObject.f90
	@$(F90) $(F90FLAGS) swirlClassObject.f90
#----------------------------------BLAS------------------------------------------

$(BLAS_LOCATION)/dgemv.o: \
   $(BLAS_LOCATION)/dgemv.f
	 (cd $(BLAS_LOCATION); \
         make dgemv.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(F77FLAGS)" )

#----------------------------------LAPACK------------------------------------------

$(LAPACK_LOCATION)/zggev.o: \
   $(LAPACK_LOCATION)/zggev.f
	 (cd $(LAPACK_LOCATION); \
         make zggev.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(F77FLAGS)" )

$(LAPACK_LOCATION)/f90_zggev.o: \
   $(LAPACK_LOCATION)/f90_zggev.f90
	 (cd $(LAPACK_LOCATION); \
         make f90_zggev.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(F77FLAGS)" )
#-----------------------------------FITPACK----------------------------------------

$(FITPACK_LOCATION)/curv1.o: \
   $(FITPACK_LOCATION)/curv1.f
	 (cd $(FITPACK_LOCATION); \
         make curv1.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(F77FLAGS)" )

$(FITPACK_LOCATION)/curv2.o: \
   $(FITPACK_LOCATION)/curv2.f
	 (cd $(FITPACK_LOCATION); \
         make curv2.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(F77FLAGS)" )

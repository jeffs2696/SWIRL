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

F90      = gfortran
#-O -fbounds-check -fcheck-array-temporaries -Wconversion -Wsurprising -Wall
F90FLAGS  =	\
			-g -c \
			-O0 \
			-std=f2008 \
			-fbacktrace -fcheck=all -fbounds-check -fcheck-array-temporaries \
			-ffpe-trap=denormal,invalid,zero,overflow,underflow \
			-fdump-fortran-global \
			-fimplicit-none \
			-Wall -Wline-truncation -Wcharacter-truncation -Wuninitialized  \
			-Wconversion -Wsurprising -Warray-temporaries -Wconversion-extra -Wextra -Werror \
			-Wno-error=unused-function \
			-Wno-error=compare-reals \
			-Wno-error=character-truncation \
			-Wno-error=conversion-extra \
			-Wno-error=conversion \
			-Wno-error=unused-dummy-argument \
			-Wno-error=do-subscript
			
F77      = gfortran
F77FLAGS  = \
			-g -c \
			-O0 \
			-std=legacy \
			-fbacktrace -fcheck=all -fbounds-check -fcheck-array-temporaries \
			-ffpe-trap=denormal,invalid,zero,overflow,underflow \
			-fdump-fortran-global \
			-fimplicit-none \
			-Wall -Wline-truncation -Wcharacter-truncation -Wuninitialized  \
			-Wconversion -Wsurprising -Warray-temporaries -Wconversion-extra -Wextra -Werror \
			-Wno-error=compare-reals \
			-Wno-error=array-temporaries \
			-Wno-error=unused-dummy-argument \
			-Wno-error=do-subscript




#-Wno-error=do-subscript \ -Wno-error=array-temporaries \ -Wno-error=conversion-extra 
# -Wno-error=do-subscript
			# -Wno-error=unused-dummy-argument\
#F90FLAGS   = -c -g -C -qextchk -qflttrap -qhsflt -qxflag=dd24 \
#	-v  -bloadmap:load.map
#
FEXT     = f
F90EXT   = f90
OEXT     = o
OBJECTS  =                     \
    main.o					   \
	swirlClassObject.o 		   \
	mmsClassObject.o		   \
    FindResidualVectorModule.o \
	SourceTermModule.o         \
	L2NormModule.o			   \
	derivsModule.o 			   \
	globalModule.o 			   \
	gridModule.o   			   \
	analysisModule.o 		   \
	boundaryModule.o 		   \
	indexxModule.o 			   \
	rmachModule.o 			   \
	smachAndSndspdModule.o     \
    machoutModule.o 		   \
	fdgridModule.o 			   \
   	fdrivsModule.o 			         
		
MODULES = $(OBJECTS:$(OEXT)=mod)

EXECUTABLE_LOCATION = ../CodeRun

EXE = swirlF2020

LIB_FILES = ../LibraryFiles
MOD_FILES = ../ModuleFiles
OBJ_FILES = ../ObjectFiles
EXE_FILES = $(EXECUTABLE_LOCATION)

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

	@(mv $(EXE) $(EXECUTABLE_LOCATION)); (mv $(OBJECTS) $(OBJ_FILES)); (mv *.mod $(MOD_FILES));
	@(cd $(EXECUTABLE_LOCATION); ./$(EXE))
	
	#@(cd $(EXECUTABLE_LOCATION); (make plot ))
plot:
	@(cd $(EXECUTABLE_LOCATION); (make plot ))

clean:
	rm -f *.o *.c *.a *.mod

cleanAll:
	rm -f *.o *.c *.a *.mod;                         \
       (cd $(FITPACK_LOCATION); rm -f *.o *.c *.a *.mod); \
       (cd $(LAPACK_LOCATION); rm -f *.o *.c *.a *.mod); \
       (cd $(BLAS_LOCATION); rm -f *.o *.c *.a *.mod)
                                                               
main.o: FortranFiles/main.f90        \
	FortranFiles/analysisModule.o \
	FortranFiles/boundaryModule.o \
	FortranFiles/derivsModule.o   \
	FortranFiles/fdgridModule.o   \
	FortranFiles/fdrivsModule.o   \
	FortranFiles/globalModule.o   \
	FortranFiles/gridModule.o     \
	FortranFiles/machoutModule.o  \
	FortranFiles/rmachModule.o    \
	FortranFiles/smachAndSndspdModule.o \
	FortranFiles/FindResidualVectorModule.o \
	FortranFiles/SourceTermModule.o  \
	FortranFiles/L2NormModule.o         \
	FortranFiles/mmsClassObject.o \
	FortranFiles/swirlClassObject.o 
	@$(F90) $(F90FLAGS) FortranFiles/main.f90

FortranFiles/analysisModule.o:  FortranFiles/analysisModule.f90 \
	$(LAPACK_LOCATION)/zggev.o\
	$(LAPACK_LOCATION)/f90_zggev.o 
	$(F90) $(F90FLAGS) -I$(LAPACK_LOCATION) FortranFiles/analysisModule.f90

FortranFiles/boundaryModule.o:  FortranFiles/boundaryModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/boundaryModule.f90

FortranFiles/derivsModule.o:  FortranFiles/derivsModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/derivsModule.f90

FortranFiles/globalModule.o:  FortranFiles/globalModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/globalModule.f90

FortranFiles/gridModule.o:  FortranFiles/gridModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/gridModule.f90

FortranFiles/fdgridModule.o:  FortranFiles/fdgridModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/fdgridModule.f90

FortranFiles/fdrivsModule.o:  FortranFiles/fdrivsModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/fdrivsModule.f90

indexxModule.o:  FortranFiles/indexxModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/indexxModule.f90

FortranFiles/machoutModule.o:  FortranFiles/machoutModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/machoutModule.f90

FortranFiles/rmachModule.o:  FortranFiles/rmachModule.f90 
	@$(F90) $(F90FLAGS) FortranFiles/rmachModule.f90

FortranFiles/smachModule.o:  FortranFiles/smachModule.f90 
	@$(F90) $(F90FLAGS) FortranFiles/smachModule.f90

FortranFiles/smachAndSndspdModule.o:  FortranFiles/smachAndSndspdModule.f90 
	@$(F90) $(F90FLAGS) FortranFiles/smachAndSndspdModule.f90

FortranFiles/sndspdModule.o:  FortranFiles/sndspdModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/sndspdModule.f90

FortranFiles/SourceTermModule.o:  FortranFiles/SourceTermModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/SourceTermModule.f90

FortranFiles/mmsClassObject.o: FortranFiles/mmsClassObject.f90
	@$(F90) $(F90FLAGS) FortranFiles/mmsClassObject.f90

FortranFiles/L2NormModule.o: FortranFiles/L2NormModule.f90
	@$(F90) $(F90FLAGS) FortranFiles/L2NormModule.f90 

FortranFiles/FindResidualVectorModule.o: FortranFiles/FindResidualVectorModule.f90\
							FortranFiles/L2NormModule.o
	@$(F90) $(F90FLAGS) FortranFiles/FindResidualVectorModule.f90

FortranFiles/swirlClassObject.o: FortranFiles/swirlClassObject.f90
	@$(F90) $(F90FLAGS) FortranFiles/swirlClassObject.f90
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

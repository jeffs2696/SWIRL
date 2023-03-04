F90 = gfortran
F90FLAGS  =	\
			-g -c \
			-O0 \
			-std=f2008 \
			-fbacktrace -fcheck=all -fbounds-check -fcheck-array-temporaries \
			-fimplicit-none \
			-ffpe-trap=denormal,invalid,zero,overflow,underflow \
			-Wall -Wline-truncation -Wcharacter-truncation -Wuninitialized  \
			-Wsurprising -Warray-temporaries \
			-Wextra \
			-Werror \
			-Wno-compare-reals \
			-Wno-unused-parameter \
			-Wno-unused-dummy-argument \
			-Wno-unused-variable\
			-Wno-array-temporaries \
			-Wno-unused-function \
			-fdump-fortran-global \
			-Wno-error=unused-parameter \
			-Wno-error=conversion-extra \
			-Wno-error=conversion \
			-Wno-error=compare-reals \
			-Wno-error=character-truncation \
			-Wno-error=do-subscript 


LIB_FILES = ../../../LibraryFiles

EXE = eigensolver
OBJECTS = \
		  eigensolver.o \
		  L2NormModule.o


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

all: $(OBJECTS)
	@$(F90) -o $(EXE) $(BLAS_OBJS) $(LAPACK_OBJS) $(FITPACK_OBJS) $(OBJECTS);
	@./$(EXE);

eigensolver.o: eigensolver.f90 \
	$(LAPACK_LOCATION)/zggev.o \
	../L2NormModule.o
	$(F90) $(F90FLAGS) -I$(LAPACK_LOCATION) eigensolver.f90 

../L2NormModule.o: ../L2NormModule.f90
	@$(F90) $(F90FLAGS) ../L2NormModule.f90 


#----------------------------------BLAS------------------------------------------

$(BLAS_LOCATION)/dgemv.o: \
   $(BLAS_LOCATION)/dgemv.f
	 @(cd $(BLAS_LOCATION); \
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


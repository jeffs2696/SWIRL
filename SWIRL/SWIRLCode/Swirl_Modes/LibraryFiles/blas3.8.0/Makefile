#
#  Makefile for BLAS
#
F77    = gfortran
FFLAGS = -c -O
FEXT   = f
OEXT   = o
OBJS   =          \
         daxpy.o  \
         ddot.o   \
         dgemv.o  \
         dscal.o  \
         dznrm2.o \
         izamax.o \
         lsame.o  \
         zcopy.o  \
         zdscal.o \
         zgemm.o  \
         zgemv.o  \
         zgerc.o  \
         zscal.o  \
         zswap.o  \
         ztrmm.o  \
         ztrmv.o    

#zblas1.o zblas2.o zblas3.o dgemv.o ddot.o daxpy.o dscal.o

LAPACK_LOCATION = ../lapack3.8.0

all:  $(OBJS)

daxpy.o: \
  daxpy.f
	$(F77) $(FFLAGS) daxpy.f

dcabs1.o: \
  dcabs1.f
	$(F77) $(FFLAGS) dcabs1.f

ddot.o: \
  ddot.f
	$(F77) $(FFLAGS) ddot.f

dgemv.o:  \
  dgemv.f \
  lsame.o \
  $(LAPACK_LOCATION)/xerbla.o
	$(F77) $(FFLAGS) -I$(LAPACK_LOCATION) dgemv.f

dscal.o: \
  dscal.f
	$(F77) $(FFLAGS) dscal.f

dznrm2.o: \
  dznrm2.f
	$(F77) $(FFLAGS) dznrm2.f

izamax.o: \
  izamax.f \
  dcabs1.o
	$(F77) $(FFLAGS) izamax.f

lsame.o: \
  lsame.f
	$(F77) $(FFLAGS) lsame.f

zcopy.o: \
  zcopy.f
	$(F77) $(FFLAGS) zcopy.f

zdscal.o: \
  zdscal.f
	$(F77) $(FFLAGS) zdscal.f

zgemm.o:  \
  zgemm.f \
  lsame.o \
  $(LAPACK_LOCATION)/xerbla.o
	$(F77) $(FFLAGS) -I$(LAPACK_LOCATION) zgemm.f

zgemv.o:  \
  zgemv.f \
  lsame.o \
  $(LAPACK_LOCATION)/xerbla.o
	$(F77) $(FFLAGS) -I$(LAPACK_LOCATION) zgemv.f

zgerc.o:  \
  zgerc.f \
  $(LAPACK_LOCATION)/xerbla.o
	$(F77) $(FFLAGS) -I$(LAPACK_LOCATION) zgerc.f

zscal.o: \
  zscal.f
	$(F77) $(FFLAGS) zscal.f

zswap.o: \
  zswap.f
	$(F77) $(FFLAGS) zswap.f

ztrmm.o:  \
  ztrmm.f \
  lsame.o \
  $(LAPACK_LOCATION)/xerbla.o
	$(F77) $(FFLAGS) -I$(LAPACK_LOCATION) ztrmm.f

ztrmv.o:  \
  ztrmv.f \
  lsame.o \
  $(LAPACK_LOCATION)/xerbla.o
	$(F77) $(FFLAGS) -I$(LAPACK_LOCATION) ztrmv.f

$(LAPACK_LOCATION)/xerbla.o: \
  $(LAPACK_LOCATION)/xerbla.f
	(cd $(LAPACK_LOCATION); \
         make xerbla.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

clean: 
	rm -f *.o *.a core

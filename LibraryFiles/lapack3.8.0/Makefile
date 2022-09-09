#
#  Makefile for LAPACK3.8.0 routines
#
F77    = gfortran
FFLAGS = -c -O
FEXT   = f
OEXT   = o
OBJS   =                \
		 f90_zggev.o \
         zggev.o        \
         disnan.o       \
         dlabad.o       \
         dladiv.o       \
         dlaisnan.o     \
         dlamch.o       \
         dlapy2.o       \
         dlapy3.o       \
         ieeeck.o       \
         ilaenv.o       \
         ilazlc.o       \
         ilazlr.o       \
         iparam2stage.o \
         iparmq.o       \
         xerbla.o       \
         zgeqr2.o       \
         zgeqrf.o       \
         zggbak.o       \
         zggbal.o       \
         zgghrd.o       \
         zhgeqz.o       \
         zlacgv.o       \
         zlacpy.o       \
         zladiv.o       \
         zlange.o       \
         zlanhs.o       \
         zlarf.o        \
         zlarfb.o       \
         zlarfg.o       \
         zlarft.o       \
         zlartg.o       \
         zlascl.o       \
         zlaset.o       \
         zlassq.o       \
         zrot.o         \
         ztgevc.o       \
         zung2r.o       \
         zungqr.o       \
         zunm2r.o       \
         zunmqr.o

#dlamc2.o  dlamch.o  xerbla.o  zggbak.o  zlacgv.o  zlanhs.o \
#zlarft.o  zlassq.o  zungqr.o  dlabad.o  dlamc3.o  dlapy3.o  zgegv.o  \
#zggbal.o  zlacpy.o  zlarf.o   zlartg.o  zrot.o    zunm2r.o  dladiv.o \
#dlamc4.o  ilaenv.o  zgeqr2.o  zgghrd.o  zladiv.o  zlarfb.o  zlascl.o \
#ztgevc.o  zunmqr.o  dlamc1.o  dlamc5.o  lsame.o   zgeqrf.o  zhgeqz.o \
#zlange.o  zlarfg.o  zlaset.o  zung2r.o  

BLAS_LOCATION = ../blas3.8.0

all:  $(OBJS)

disnan.o:  \
  disnan.f \
  dlaisnan.o
	$(F77) $(FFLAGS) disnan.f

dlabad.o: \
  dlabad.f
	$(F77) $(FFLAGS) dlabad.f

dladiv.o:   \
  dladiv.f  \
  dlamch.o
	$(F77) $(FFLAGS) dladiv.f

dlaisnan.o: \
  dlaisnan.f
	$(F77) $(FFLAGS) dlaisnan.f

dlamch.o: \
  dlamch.f
	$(F77) $(FFLAGS) dlamch.f

dlapy2.o:  \
  dlapy2.f \
  disnan.o
	$(F77) $(FFLAGS) dlapy2.f

dlapy3.o: \
  dlapy3.f
	$(F77) $(FFLAGS) dlapy3.f

ieeeck.o: \
  ieeeck.f
	$(F77) $(FFLAGS) ieeeck.f

ilaenv.o:   \
  ilaenv.f  \
  ieeeck.o \
  iparam2stage.o \
  iparmq.o
	$(F77) $(FFLAGS) ilaenv.f

ilazlc.o: \
  ilazlc.f
	$(F77) $(FFLAGS) ilazlc.f

ilazlr.o: \
  ilazlr.f
	$(F77) $(FFLAGS) ilazlr.f

iparam2stage.o: \
  iparam2stage.f
	$(F77) $(FFLAGS) iparam2stage.f

iparmq.o: \
  iparmq.f
	$(F77) $(FFLAGS) iparmq.f

xerbla.o: \
  xerbla.f
	$(F77) $(FFLAGS) xerbla.f

zlacgv.o: \
  zlacgv.f
	$(F77) $(FFLAGS) zlacgv.f

zlacpy.o:                   \
  zlacpy.f                  \
  $(BLAS_LOCATION)/lsame.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlacpy.f

zladiv.o: \
  zladiv.f \
  dladiv.o
	$(F77) $(FFLAGS) zladiv.f

zlanhs.o:                   \
  zlanhs.f                  \
  disnan.o                  \
  $(BLAS_LOCATION)/lsame.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlanhs.f

zlarf.o:                    \
  zlarf.f                   \
  ilazlr.o                  \
  ilazlc.o                  \
  $(BLAS_LOCATION)/zgemv.o  \
  $(BLAS_LOCATION)/zgerc.o  \
  $(BLAS_LOCATION)/lsame.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlarf.f

zlarfb.o:                   \
  zlarfb.f                  \
  zlacgv.o                  \
  $(BLAS_LOCATION)/zcopy.o  \
  $(BLAS_LOCATION)/zgemm.o  \
  $(BLAS_LOCATION)/ztrmm.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlarfb.f

zlarfg.o:                   \
  zlarfg.f                  \
  dlamch.o                  \
  dlapy3.o                  \
  zladiv.o                  \
  $(BLAS_LOCATION)/dznrm2.o \
  $(BLAS_LOCATION)/zdscal.o \
  $(BLAS_LOCATION)/zscal.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlarfg.f

zlarft.o:                  \
  zlarft.f                 \
  $(BLAS_LOCATION)/lsame.o \
  $(BLAS_LOCATION)/zgemm.o \
  $(BLAS_LOCATION)/zgemv.o \
  $(BLAS_LOCATION)/ztrmv.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlarft.f

zlartg.o:                   \
  zlartg.f                  \
  dlamch.o                  \
  dlapy2.o                  \
  disnan.o
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlartg.f

zlascl.o:                   \
  zlascl.f                  \
  dlamch.o                  \
  disnan.o                  \
  xerbla.o                  \
  $(BLAS_LOCATION)/lsame.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlascl.f

zlaset.o:                    \
  zlaset.f                   \
  $(BLAS_LOCATION)/lsame.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlaset.f

zgeqr2.o:                   \
  zgeqr2.f                  \
  zlarf.o                   \
  zlarfb.o                  \
  zlarfg.o                  \
  xerbla.o
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zgeqr2.f

zgeqrf.o:                   \
  zgeqrf.f                  \
  ilaenv.o                  \
  zgeqr2.o                  \
  zlarfb.o                  \
  zlarft.o                  \
  xerbla.o
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zgeqrf.f

zggbak.o:                   \
  zggbak.f                  \
  $(BLAS_LOCATION)/lsame.o  \
  $(BLAS_LOCATION)/zdscal.o \
  $(BLAS_LOCATION)/zswap.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zggbak.f

zggbal.o:                   \
  zggbal.f                  \
  dlamch.o                  \
  xerbla.o                  \
  $(BLAS_LOCATION)/daxpy.o  \
  $(BLAS_LOCATION)/ddot.o   \
  $(BLAS_LOCATION)/dscal.o  \
  $(BLAS_LOCATION)/izamax.o \
  $(BLAS_LOCATION)/lsame.o  \
  $(BLAS_LOCATION)/zdscal.o \
  $(BLAS_LOCATION)/zswap.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zggbal.f

f90_zggev.o:                    \
	f90_zggev.f90           \
	zggev.f                   \
	dlabad.o                  \
	dlamch.o                  \
	ilaenv.o                  \
	xerbla.o                  \
	zgeqrf.o                  \
	zggbal.o                  \
	zgghrd.o                  \
	zhgeqz.o                  \
	zlacpy.o                  \
	zlange.o                  \
	ztgevc.o                  \
	zungqr.o                  \
	zunmqr.o                  \
	$(BLAS_LOCATION)/lsame.o
	$(F77) $(FFLAGS) f90_zggev.f90

zggev.o:                    \
  zggev.f                   \
  dlabad.o                  \
  dlamch.o                  \
  ilaenv.o                  \
  xerbla.o                  \
  zgeqrf.o                  \
  zggbal.o                  \
  zgghrd.o                  \
  zhgeqz.o                  \
  zlacpy.o                  \
  zlange.o                  \
  ztgevc.o                  \
  zungqr.o                  \
  zunmqr.o                  \
  $(BLAS_LOCATION)/lsame.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zggev.f

zgghrd.o:                    \
  zgghrd.f                   \
  xerbla.o                  \
  zlartg.o                  \
  zlaset.o                  \
  zrot.o                    \
  $(BLAS_LOCATION)/lsame.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zgghrd.f

zhgeqz.o:                   \
  zhgeqz.f                  \
  dlamch.o                  \
  xerbla.o                  \
  zlanhs.o                  \
  zlartg.o                  \
  zrot.o                    \
  $(BLAS_LOCATION)/lsame.o  \
  $(BLAS_LOCATION)/zscal.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zhgeqz.f

zlange.o:                   \
  zlange.f                  \
  disnan.o                  \
  zlassq.o                  \
  $(BLAS_LOCATION)/lsame.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlange.f

zlassq.o:                   \
  zlassq.f                  \
  disnan.o
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zlassq.f

zrot.o: \
  zrot.f
	$(F77) $(FFLAGS) zrot.f

ztgevc.o:                   \
  ztgevc.f                  \
  dlabad.o                  \
  dlamch.o                  \
  zggbak.o                  \
  zladiv.o                  \
  zlascl.o                  \
  xerbla.o                  \
  $(BLAS_LOCATION)/lsame.o  \
  $(BLAS_LOCATION)/zgemv.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) ztgevc.f

zung2r.o:                   \
  zung2r.f                  \
  xerbla.o                  \
  zlarf.o                   \
  $(BLAS_LOCATION)/zscal.o 
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zung2r.f

zungqr.o:                   \
  zungqr.f                  \
  ilaenv.o                  \
  xerbla.o                  \
  zlarfb.o                  \
  zlarft.o                  \
  zung2r.o
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zungqr.f

zunm2r.o:                   \
  zunm2r.f                  \
  xerbla.o                  \
  zlarf.o                   \
  $(BLAS_LOCATION)/lsame.o
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zunm2r.f

zunmqr.o:                   \
  zunmqr.f                  \
  ilaenv.o                  \
  xerbla.o                  \
  zlarfb.o                  \
  zlarft.o                  \
  zunm2r.o                  \
  $(BLAS_LOCATION)/lsame.o
	$(F77) $(FFLAGS) -I$(BLAS_LOCATION) zunmqr.f

#---------------------BLAS routines---------------------------------

$(BLAS_LOCATION)/daxpy.o: \
  $(BLAS_LOCATION)/daxpy.f 
	(cd $(BLAS_LOCATION); \
         make daxpy.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/ddot.o: \
  $(BLAS_LOCATION)/ddot.f 
	(cd $(BLAS_LOCATION); \
         make ddot.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/dscal.o: \
  $(BLAS_LOCATION)/dscal.f 
	(cd $(BLAS_LOCATION); \
         make dscal.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/dznrm2.o: \
  $(BLAS_LOCATION)/dznrm2.f 
	(cd $(BLAS_LOCATION); \
         make dznrm2.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/izamax.o: \
  $(BLAS_LOCATION)/izamax.f 
	(cd $(BLAS_LOCATION); \
         make izamax.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/lsame.o: \
  $(BLAS_LOCATION)/lsame.f 
	(cd $(BLAS_LOCATION); \
         make lsame.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/zcopy.o: \
  $(BLAS_LOCATION)/zcopy.f 
	(cd $(BLAS_LOCATION); \
         make zcopy.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/zdscal.o: \
  $(BLAS_LOCATION)/zdscal.f 
	(cd $(BLAS_LOCATION); \
         make zdscal.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/zgemm.o: \
  $(BLAS_LOCATION)/zgemm.f 
	(cd $(BLAS_LOCATION); \
         make zgemm.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/zgemv.o: \
  $(BLAS_LOCATION)/zgemv.f 
	(cd $(BLAS_LOCATION); \
         make zgemv.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/zgerc.o: \
  $(BLAS_LOCATION)/zgerc.f 
	(cd $(BLAS_LOCATION); \
         make zgerc.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/zlacgv.o: \
  $(BLAS_LOCATION)/zlacgv.f 
	(cd $(BLAS_LOCATION); \
         make zlacgv.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/zscal.o: \
  $(BLAS_LOCATION)/zscal.f 
	(cd $(BLAS_LOCATION); \
         make zscal.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/zswap.o: \
  $(BLAS_LOCATION)/zswap.f 
	(cd $(BLAS_LOCATION); \
         make zswap.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/ztrmm.o: \
  $(BLAS_LOCATION)/ztrmm.f 
	(cd $(BLAS_LOCATION); \
         make ztrmm.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

$(BLAS_LOCATION)/ztrmv.o: \
  $(BLAS_LOCATION)/ztrmv.f 
	(cd $(BLAS_LOCATION); \
         make ztrmv.o "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(FFLAGS)" )

clean: 
	rm -f *.o *.a *.mod core

cleanAll: 
	rm -f *.o *.a core;   \
        (cd $(BLAS_LOCATION); \
         rm -f *.o *.a core)
    

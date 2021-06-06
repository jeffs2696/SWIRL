#
#  Makefile for LAPACK
#
F77    = f77
FFLAGS = -c -O
FEXT   = f
OEXT   = o
OBJS   =    \
	dlamc2.o  dlamch.o  xerbla.o  zggbak.o  zlacgv.o  zlanhs.o \
	zlarft.o  zlassq.o  zungqr.o  dlabad.o  dlamc3.o  dlapy3.o  zgegv.o  \
	zggbal.o  zlacpy.o  zlarf.o   zlartg.o  zrot.o    zunm2r.o  dladiv.o \
	dlamc4.o  ilaenv.o  zgeqr2.o  zgghrd.o  zladiv.o  zlarfb.o  zlascl.o \
	ztgevc.o  zunmqr.o  dlamc1.o  dlamc5.o  lsame.o   zgeqrf.o  zhgeqz.o \
	zlange.o  zlarfg.o  zlaset.o  zung2r.o  

all:  $(OBJS)

.$(FEXT).$(OEXT):
	$(F77) $(FFLAGS) $<

clean: 
	rm -f *.o *.a core

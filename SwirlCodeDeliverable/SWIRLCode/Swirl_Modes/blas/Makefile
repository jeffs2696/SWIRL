#
#  Makefile for BLAS
#
F77    = f77
FFLAGS = -c -O
FEXT   = f
OEXT   = o
OBJS   =   \
	zblas1.o zblas2.o zblas3.o dgemv.o ddot.o daxpy.o dscal.o

all:  $(OBJS)

.$(FEXT).$(OEXT):   ; $(F77) $(FFLAGS) $<

clean: 
	rm -f *.o *.a core

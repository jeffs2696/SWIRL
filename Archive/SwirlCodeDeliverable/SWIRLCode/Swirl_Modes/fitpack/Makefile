#
#  Makefile for FITPACK
#
F77    = f77
FFLAGS = -c -O
FEXT   = f
OEXT   = o
OBJS   =   \
	ceez.o curv1.o curv2.o intrvl.o snhcsh.o terms.o

all:  $(OBJS)

#.$(FEXT).$(OEXT):
#	$(F77) $(FFLAGS) $<

ceez.o:  \
  ceez.f \
  snhcsh.o  \
  terms.o
	$(F77) $(FFLAGS) ceez.f

curv1.o:  \
  curv1.f \
  ceez.o  \
  terms.o
	$(F77) $(FFLAGS) curv1.f

curv2.o:   \
  curv2.f  \
  intrvl.o \
  snhcsh.o 
	$(F77) $(FFLAGS) curv2.f

intrvl.o:  \
  intrvl.f
	$(F77) $(FFLAGS) intrvl.f

snhcsh.o:  \
  snhcsh.f
	$(F77) $(FFLAGS) snhcsh.f

terms.o:  \
  terms.f \
  snhcsh.o 
	$(F77) $(FFLAGS) terms.f

clean: 
	rm -f *.o *.a core

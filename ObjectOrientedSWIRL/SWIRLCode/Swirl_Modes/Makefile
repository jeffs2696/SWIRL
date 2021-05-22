##################################################################
#
#   Top level Makefile for SWIRL
#   Version 3.2
#
#   Kenneth A. Kousen
#   United Technologies Research Center
#   411 Silver Lane, MS 129-20
#   East Hartford, CT  06118
#   kousen@utrc.utc.com
#   March 25, 1996
#
# modified to add the F2008 version of SWIRL
#
#   Ray Hixon
#   U. Toledo
#   October 19, 2016
#
# modified to account for changing folders into neater directories
#
#   Jeff Severino
#   U. Toledo
#   May, 21, 2021
###################################################################
#
EXE77     = swirl
EXE90     = swirlF2008
#LINK    = f77
LINK    = gfortran
LFLAG   = -o
#LFLAG   = -bloadmap:load.map -o 
#F77     = f77
F77     = gfortran
#F77FLAGS  = -c -O
F77FLAGS  = -c -O -fbounds-check -fcheck-array-temporaries -Wconversion -Wsurprising -Wall 
F90     = gfortran
F90FLAGS  = -c -O -std=f2008 -fbounds-check -fcheck-array-temporaries -Wconversion -Wsurprising -Wall# -Werror
#F90FLAGS  = -c -O
#FFLAGS  = -c -O
#FFLAGS  = -c -g
FEXT    = f
OEXT    = o


OBJSRC90 = \
		   srcF2008/swirl.o          \
		   srcF2008/derivsModule.o   \
		   srcF2008/globalModule.o   \
		   srcF2008/gridModule.o     \
		   srcF2008/inputModule.o    \
		   srcF2008/analysisModule.o \
		   srcF2008/boundaryModule.o \
		   srcF2008/outputModule.o   \
		   srcF2008/rmachModule.o    \
		   srcF2008/smachAndSndspdModule.o \
		   srcF2008/machoutModule.o                   \
		   srcF2008/fdgridModule.o \
		   srcF2008/fdrivsModule.o \
		   srcF2008/kapsubModule.o    \
		   srcF2008/interpModule.o \
		   srcF2008/indexxModule.o \
		   srcF2008/Akima1D.o         \
		   srcF2008/akima433.o

OBJSRC  =     \
			  src/swirl.o \
			  src/derivs.o \
			  src/global.o \
			  src/grid.o \
			  src/input.o       \
			  src/analysis.o \
			  src/boundary.o \
			  src/output.o \
			  src/rmach.o  \
			  src/smach.o \
			  src/machout.o \
			  src/sndspd.o \
			  src/fdgrid.o \
			  src/fdrivs.o \
			  src/kapsub.o  \
			  src/interp.o \
			  src/indexx.o

OBJBLAS =     \
			  blas/zblas1.o    blas/zblas2.o    blas/zblas3.o    blas/dgemv.o    \
			  blas/ddot.o      blas/daxpy.o     blas/dscal.o
OBJLPCK =     \
			  lapack/dlamc2.o  lapack/dlamch.o  lapack/xerbla.o  lapack/zggbak.o \
			  lapack/zlacgv.o  lapack/zlanhs.o  lapack/zlarft.o  lapack/zlassq.o \
			  lapack/zungqr.o  lapack/dlabad.o  lapack/dlamc3.o  lapack/dlapy3.o \
			  lapack/zgegv.o   lapack/zggbal.o  lapack/zlacpy.o  lapack/zlarf.o  \
			  lapack/zlartg.o  lapack/zrot.o    lapack/zunm2r.o  lapack/dladiv.o \
			  lapack/dlamc4.o  lapack/ilaenv.o  lapack/zgeqr2.o  lapack/zgghrd.o \
			  lapack/zladiv.o  lapack/zlarfb.o  lapack/zlascl.o  lapack/ztgevc.o \
			  lapack/zunmqr.o  lapack/dlamc1.o  lapack/dlamc5.o  lapack/lsame.o  \
			  lapack/zgeqrf.o  lapack/zhgeqz.o  lapack/zlange.o  lapack/zlarfg.o \
			  lapack/zlaset.o  lapack/zung2r.o  
OBJFTPK =     \
			  fitpack/ceez.o   fitpack/curv1.o  fitpack/curv2.o  fitpack/intrvl.o \
			  fitpack/snhcsh.o fitpack/terms.o
OBJECTS =     \
			  $(OBJSRC) $(OBJBLAS) $(OBJLPCK) $(OBJFTPK)
OBJECTS90 =     \
				$(OBJSRC90) $(OBJBLAS) $(OBJLPCK) $(OBJFTPK)


$(EXE90): 
	(cd SourceFiles/srcF2008; make "F77=$(F77)" "F90=$(F90)" "LINK=$(LINK)" "F77FLAGS=$(F77FLAGS)" "F90FLAGS=$(F90FLAGS)" "LFLAG=$(LFLAG)")

$(EXE77): $(OBJECTS)
	$(LINK) $(OBJECTS) $(LFLAG) $(EXE77); mv $(EXE77) CodeRun

$(OBJSRC):
	(cd SourceFiles/src; make "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(F77FLAGS)" "LFLAG=$(LFLAG)")

$(OBJSRC90):
	(cd SourceFiles/srcF2008; make "F77=$(F77)" "F90=$(F90)" "LINK=$(LINK)" "F77FLAGS=$(F77FLAGS)" "F90FLAGS=$(F90FLAGS)" "LFLAG=$(LFLAG)")

$(OBJBLAS):
	(cd LibraryFiles/blas; make "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(F77FLAGS)" "LFLAG=$(LFLAG)")

$(OBJLPCK):
	(cd LibraryFiles/lapack; make "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(F77FLAGS)" "LFLAG=$(LFLAG)")

$(OBJFTPK):
	(cd LibraryFiles/fitpack; make "F77=$(F77)" "LINK=$(LINK)" "FFLAGS=$(F77FLAGS)" "LFLAG=$(LFLAG)")

clean:
	rm -f CodeRun/$(EXE77)
	rm -f CodeRun/$(EXE90)
	rm -f $(EXE77) core
	rm -f src/*.o
	rm -f srcF2008/*.o
	rm -f srcF2008/*.mod
	rm -f blas/*.o
	rm -f lapack/*.o
	rm -f fitpack/*.o
	#
	#   Define dependency information for object files
	#                                                               
SourceFiles/src/swirl.o: src/swirl.f \
		src/pmnmax
SourceFiles/src/analysis.o: src/analysis.f \
	src/pmnmax
SourceFiles/src/boundary.o: src/boundary.f \
	src/pmnmax
SourceFiles/src/derivs.o: src/derivs.f \
	src/pmnmax
SourceFiles/src/global.o: src/global.f \
	src/pmnmax
SourceFiles/src/grid.o:  src/grid.f \
	src/pmnmax
SourceFiles/src/input.o: src/input.f
SourceFiles/src/fdgrid.o: src/fdgrid.f \
	src/pmnmax
SourceFiles/src/fdrivs.o: src/fdrivs.f \
	src/pmnmax
SourceFiles/src/indexx.o: src/indexx.f
SourceFiles/src/interp.o: src/interp.f \
	src/pmnmax
SourceFiles/src/kapsub.o: src/kapsub.f
SourceFiles/src/machout.o: src/machout.f \
	src/pmnmax
SourceFiles/src/output.o: src/output.f \
	src/pmnmax
SourceFiles/src/rmach.o: src/rmach.f \
	src/pmnmax
SourceFiles/src/smach.o: src/smach.f \
	src/pmnmax
SourceFiles/src/sndspd.o: src/sndspd.f \
	src/pmnmax
SourceFiles/srcF2008/swirl.o: srcF2008/swirl.f90
SourceFiles/srcF2008/akima433.o: srcF2008/akima433.f
SourceFiles/srcF2008/Akima1D.o: srcF2008/Akima1D.f90
SourceFiles/srcF2008/boundaryModule.o: srcF2008/boundaryModule.f90
SourceFiles/srcF2008/derivsModule.o: srcF2008/derivsModule.f90
SourceFiles/srcF2008/fdgridModule.o: srcF2008/fdgridModule.f90
SourceFiles/srcF2008/fdrivsModule.o: srcF2008/fdrivsModule.f90
SourceFiles/srcF2008/gridModule.o: srcF2008/gridModule.f90
SourceFiles/srcF2008/globalModule.o: srcF2008/globalModule.f90
SourceFiles/srcF2008/indexxModule.o: srcF2008/indexxModule.f90
SourceFiles/srcF2008/interpModule.o: srcF2008/interpModule.f90
SourceFiles/srcF2008/kapsubModule.o: srcF2008/kapsubModule.f90
SourceFiles/srcF2008/machoutModule.o: srcF2008/machoutModule.f90
SourceFiles/srcF2008/outputModule.o: srcF2008/outputModule.f90
SourceFiles/srcF2008/rmachModule.o: srcF2008/rmachModule.f90
SourceFiles/srcF2008/smachAndSndspdModule.o: srcF2008/smachAndSndspdModule.f90
LibraryFiles/blas/zblas1.o: blas/zblas1.f
LibraryFiles/blas/zblas2.o: blas/zblas2.f
LibraryFiles/blas/zblas3.o: blas/zblas3.f
LibraryFiles/blas/dgemv.o: blas/dgemv.f
LibraryFiles/blas/ddot.o: blas/ddot.f
LibraryFiles/blas/daxpy.o: blas/daxpy.f
LibraryFiles/blas/dscal.o: blas/dscal.f
LibraryFiles/lapack/dlamc2.o: lapack/dlamc2.f
LibraryFiles/lapack/dlamch.o: lapack/dlamch.f
LibraryFiles/lapack/xerbla.o: lapack/xerbla.f
LibraryFiles/lapack/zggbak.o: lapack/zggbak.f
LibraryFiles/lapack/zlacgv.o: lapack/zlacgv.f
LibraryFiles/lapack/zlanhs.o: lapack/zlanhs.f
LibraryFiles/lapack/zlarft.o: lapack/zlarft.f
LibraryFiles/lapack/zlassq.o: lapack/zlassq.f
LibraryFiles/lapack/zungqr.o: lapack/zungqr.f
LibraryFiles/lapack/dlabad.o: lapack/dlabad.f
LibraryFiles/lapack/dlamc3.o: lapack/dlamc3.f
LibraryFiles/lapack/dlapy3.o: lapack/dlapy3.f
LibraryFiles/lapack/zgegv.o: lapack/zgegv.f
LibraryFiles/lapack/zggbal.o: lapack/zggbal.f
LibraryFiles/lapack/zlacpy.o: lapack/zlacpy.f
LibraryFiles/lapack/zlarf.o: lapack/zlarf.f
LibraryFiles/lapack/zlartg.o: lapack/zlartg.f
LibraryFiles/lapack/zrot.o: lapack/zrot.f
LibraryFiles/lapack/zunm2r.o: lapack/zunm2r.f
LibraryFiles/lapack/dladiv.o: lapack/dladiv.f
LibraryFiles/lapack/dlamc4.o: lapack/dlamc4.f
LibraryFiles/lapack/ilaenv.o: lapack/ilaenv.f
LibraryFiles/lapack/zgeqr2.o: lapack/zgeqr2.f
LibraryFiles/lapack/zgghrd.o: lapack/zgghrd.f
LibraryFiles/lapack/zladiv.o: lapack/zladiv.f
LibraryFiles/lapack/zlarfb.o: lapack/zlarfb.f
LibraryFiles/lapack/zlascl.o: lapack/zlascl.f
LibraryFiles/lapack/ztgevc.o: lapack/ztgevc.f
LibraryFiles/lapack/zunmqr.o: lapack/zunmqr.f
LibraryFiles/lapack/dlamc1.o: lapack/dlamc1.f
LibraryFiles/lapack/dlamc5.o: lapack/dlamc5.f
LibraryFiles/lapack/lsame.o: lapack/lsame.f
LibraryFiles/lapack/zgeqrf.o: lapack/zgeqrf.f
LibraryFiles/lapack/zhgeqz.o: lapack/zhgeqz.f
LibraryFiles/lapack/zlange.o: lapack/zlange.f
LibraryFiles/lapack/zlarfg.o: lapack/zlarfg.f
LibraryFiles/lapack/zlaset.o: lapack/zlaset.f
LibraryFiles/lapack/zung2r.o: lapack/zung2r.f
LibraryFiles/fitpack/ceez.o: fitpack/ceez.f
LibraryFiles/fitpack/curv1.o: fitpack/curv1.f
LibraryFiles/fitpack/curv2.o: fitpack/curv2.f
LibraryFiles/fitpack/intrvl.o: fitpack/intrvl.f
LibraryFiles/fitpack/snhcsh.o: fitpack/snhcsh.f
LibraryFiles/fitpack/terms.o: fitpack/terms.f

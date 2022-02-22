### MAKEFILE DEFINITIONS ###

GFORT = mpif90

GFORTFLAGS_OPT = -c -O3
GFORTFLAGS2_OPT = -c -O3
GFORTLDFLAGS_OPT = -O3

GFORTFLAGS_DEBUG = -c -O0 -Wall -Wline-truncation -Wuninitialized -Wconversion \
                  -Wintrinsics-std -fcheck=all -Wsurprising -fbounds-check -std=f2008  \
                  -Wconversion-extra -Warray-temporaries -fcheck=all
GFORTFLAGS2_DEBUG = -c -O0 -Wall -Wline-truncation -Wuninitialized -Wconversion \
                  -Wintrinsics-std -fcheck=all -Wsurprising -fbounds-check -std=f2008  \
                  -Wconversion-extra -Warray-temporaries -fcheck=all
GFORTLDFLAGS_DEBUG = -O0

GFORTFLAGS_PROF = -c -O -pg
GFORTFLAGS2_PROF = -c -O -pg
GFORTLDFLAGS_PROF = -O -pg

F95       = $(GFORT)
F95FLAGS  = $(GFORTFLAGS_DEBUG)
F95FLAGS2 = $(GFORTFLAGS2_DEBUG)
LDFLAGS   = $(GFORTLDFLAGS_DEBUG)

# define locations for files

LIB_FILES = ../LibraryFiles
MOD_FILES = ../ModuleFiles
OBJ_FILES = ../ObjectFiles
EXE_FILES = ../Executables
SRC_FILES = ../SourceCode

CMD = LUSGSClass.exe

# define global paths

INTERFACE_WITH_MPI_LOCATION = ../../InterfaceWithMPI
ERROR_INFO_CLASS_LOCATION = ../../ErrorInformationClass
MPI2_DATA_CLASS_LOCATION = ../../MPI2DataClass
ORDER_OF_ACCURACY_CLASS_LOCATION = ../../OrderOfAccuracyClass
LAPACK_LOCATION = ../../LAPACK3.10.0/lapack-3.10.0

# MPICH2 definitions

MPICH2_INCLUDE_LOCATION = /usr/local/include
MPICH2_LIBRARY_LOCATION = /usr/local/lib
MPICH2_LIBRARIES =

MPICH2_TEST_RUN_CMD = mpiexec -n 2

# OpenMPI definitions

OPENMPI_INCLUDE_LOCATION = /usr/local/include
OPENMPI_LIBRARY_LOCATION = /usr/local/library
OPENMPI_LIBRARIES = -llamf77mpi \
                   -lmpi       \
                   -llam       \
                   -lutil      \
                   -ldl        \
                   -pthread

OPEN_TEST_RUN_CMD = mpirun -np 3 -s n0

# LAM MPI definitions

LAMMPI_INCLUDE_LOCATION = /usr/local/include
LAMMPI_LIBRARY_LOCATION = /usr/local/library
LAMMPI_LIBRARIES = -llamf77mpi \
                   -lmpi       \
                   -llam       \
                   -lutil      \
                   -ldl        \
                   -pthread

LAM_TEST_RUN_CMD = mpirun -np 3 -s n0

SICORMPI_INCLUDE_LOCATION = /usr/include/gentoo-multilib/n64
SICORMPI_LIBRARY_LOCATION = /usr/lib
SICORMPI_LIBRARIES = -lscmpi_debug

SICOR_TEST_RUN_CMD = srun -p sca -n 3

DEFAULT_MPI_INCLUDE_LOCATION = ../
DEFAULT_MPI_LIBRARY_LOCATION = ../
DEFAULT_MPI_LIBRARIES =
DEFAULT_MPI_TEST_RUN_CMD = mpiexec -np 2
DEFAULT_MPI_TEST_RUN_CMD_LOCAL = mpiexec -np 2

MPI_INCLUDE_LOCATION = $(DEFAULT_MPI_INCLUDE_LOCATION)
MPI_LIBRARY_LOCATION = $(DEFAULT_MPI_LIBRARY_LOCATION)
MPI_LIBRARIES = $(DEFAULT_MPI_LIBRARIES)
TEST_RUN_CMD = $(DEFAULT_MPI_TEST_RUN_CMD)
TEST_RUN_CMD_LOCAL = $(DEFAULT_MPI_TEST_RUN_CMD_LOCAL)

# define commands

MV_LIB = mv libLUSGSClass.a $(LIB_FILES)
MV_LIB_MOD = mv *.mod $(MOD_FILES)
MV_OBJ = mv *.o $(OBJ_FILES)
MV_MOD = mv *.mod $(MOD_FILES)
MV_EXE = mv $(CMD) $(EXE_FILES)

TEST_SUCCESS = LUSGSClassTest.PASSED

LUSGS_CLASS_LIBS =            \
    $(LIB_FILES)/LUSGSClass.a  \

LUSGS_CLASS_OBJS =                        \
    $(OBJ_FILES)/LUSGSClass.o 

LIBS_NEEDED = \
 $(LIB_FILES)/libMessagePassingInterface.a \
 $(LIB_FILES)/libErrorInformationClass.a \
 $(LIB_FILES)/libMPI2DataClassNoErrClass.a \
 $(LIB_FILES)/libOrderOfAccuracyClass.a \
 $(LIB_FILES)/liblapack.a

OBJS = \
 $(LUSGS_CLASS_OBJS)

TEST_OBJS =                            \
 $(OBJ_FILES)/LUSGSClassTest.o         \
 $(OBJ_FILES)/LUSGSClassTest_General.o \
 $(OBJ_FILES)/Check1DData.o            \
 $(OBJ_FILES)/Check2DData.o            \
 $(OBJ_FILES)/Check3DData.o            \
 $(OBJ_FILES)/Check4DData.o            \
 $(OBJ_FILES)/Check5DData.o            \
 $(OBJ_FILES)/LUSGSClassTest_LAPACK.o  \
 $(OBJ_FILES)/LUSGSClassTest_1D.o      \
 $(OBJ_FILES)/LUSGSClassTest_2D.o      \
 $(OBJ_FILES)/LUSGSClassTest_3D.o      \
 $(OBJ_FILES)/LUSGSClassTest_4D.o

PREVIOUS_TESTS = \
 MessagePassingInterfaceTest.PASSED \
 ErrorInformationClassTest.PASSED   \
 MPI2DataClassNoErrClassTest.PASSED \
 OrderOfAccuracyClassTest.PASSED

LIBS = \
 -lOrderOfAccuracyClass      \
 -lMPI2DataClassNoErrClass   \
 -lMessagePassingInterface   \
 -lErrorInformationClass     \
 -llapack                    \
 -lrefblas                   \
 -lLUSGSClass

MODS = \
 -I$(MOD_FILES)

MESSAGE_PASSING_INTERFACE_LIB_OBJS = \
   $(INTERFACE_WITH_MPI_LOCATION)/ObjectFiles/*.o

ERROR_INFO_CLASS_LIB_OBJS = \
   $(ERROR_INFO_CLASS_LOCATION)/ObjectFiles/*.o

MPI2_DATA_CLASS_LIB_OBJS = \
   $(MPI2_DATA_CLASS_LOCATION)/ObjectFiles/*.o

ORDER_OF_ACCURACY_CLASS_LIB_OBJS = \
   $(ORDER_OF_ACCURACY_CLASS_LOCATION)/ObjectFiles/*.o

LAPACK_LIB_OBJS = \
   $(LAPACK_LOCATION)/SRC/*.o

LAPACK_LIB_OBJS2 = \
   $(LAPACK_LOCATION)/INSTALL/*.o

BLAS_LIB_OBJS = \
   $(LAPACK_LOCATION)/BLAS/SRC/*.o

#------Commands---------------------

# Linkage rule

checkLibs:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	cd $(ERROR_INFO_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"
	cd $(ORDER_OF_ACCURACY_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" 
	cd $(LAPACK_LOCATION)/SRC; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" 
	make all


all: $(LIB_FILES)/libLUSGSClass.a
	@echo " "
	@echo "-------------------------------------------------------- "
	@echo "-   Hixon Technologies LUSGSClass routines     - "
	@echo "-    Copyright 2020 by Hixon Technologies, LLC         - "
	@echo "-    Used with permission.                             - "
	@echo "-    These routines come with no warranty.             - "
	@echo "-                                                      - "
	@echo "-   Contact:  Ray Hixon, Hixon Technologies            - "
	@echo "-             fshixon@yahoo.com                        - "
	@echo "-------------------------------------------------------- "
	@echo " "

help:
	@echo '    ' 
	@echo '  The make commands are: '
	@echo '    ' 
	@echo '   make gfortran-opt        '
	@echo '   make gfortran-debug      '
	@echo '   make gfortran-prof       '
	@echo '   make gfortranTest-opt        '
	@echo '   make gfortranTest-debug      '
	@echo '   make gfortranTest-prof       '
	@echo '    ' 

gfortran-opt:                             
	make all                          \
         "F95 = $(GFORT)"                 \
         "F95FLAGS = $(GFORTFLAGS_OPT)"   \
         "F95FLAGS2 = $(GFORTFLAGS2_OPT)" \
         "LDFLAGS = $(GFORTLDFLAGS_OPT)"

gfortran-debug:                             
	make all                            \
         "F95 = $(GFORT)"                   \
         "F95FLAGS = $(GFORTFLAGS_DEBUG)"   \
         "F95FLAGS2 = $(GFORTFLAGS2_DEBUG)" \
         "LDFLAGS = $(GFORTLDFLAGS_DEBUG)"

gfortran-prof:                             
	make all                           \
         "F95 = $(GFORT)"                  \
         "F95FLAGS = $(GFORTFLAGS_PROF)"   \
         "F95FLAGS2 = $(GFORTFLAGS2_PROF)" \
         "LDFLAGS = $(GFORTLDFLAGS_PROF)"

gfortranTest-opt:                             
	make test                         \
         "F95 = $(GFORT)"                 \
         "F95FLAGS = $(GFORTFLAGS_OPT)"   \
         "F95FLAGS2 = $(GFORTFLAGS2_OPT)" \
         "LDFLAGS = $(GFORTLDFLAGS_OPT)"

gfortranTest-debug:                             
	make test                           \
         "F95 = $(GFORT)"                   \
         "F95FLAGS = $(GFORTFLAGS_DEBUG)"   \
         "F95FLAGS2 = $(GFORTFLAGS2_DEBUG)" \
         "LDFLAGS = $(GFORTLDFLAGS_DEBUG)"

gfortranTest-prof:                             
	make test                          \
         "F95 = $(GFORT)"                  \
         "F95FLAGS = $(GFORTFLAGS_PROF)"   \
         "F95FLAGS2 = $(GFORTFLAGS2_PROF)" \
         "LDFLAGS = $(GFORTLDFLAGS_PROF)"

test: $(TEST_SUCCESS)

$(TEST_SUCCESS):  \
      $(PREVIOUS_TESTS) \
      $(LIB_FILES)/libLUSGSClass.a \
      $(TEST_OBJS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(MODS) $(LIBS) -L$(LIB_FILES) -L$(MPI_LIBRARY_LOCATION) $(MPI_LIBRARIES)
	$(MV_EXE)
	cd $(EXE_FILES); $(TEST_RUN_CMD) ./$(CMD)
	cp $(EXE_FILES)/$(TEST_SUCCESS) .

clean:
	rm -f *.a 
	rm -f *.o 
	rm -f *.d 
	rm -f *.mod 
	rm -f ifc*
	rm -f *.pc
	rm -f *.pcl
	rm -f $(LIB_FILES)/*.a
	rm -f $(MOD_FILES)/*.mod
	rm -f $(OBJ_FILES)/*.o
	rm -f $(OBJ_FILES)/*.d

cleanAll:
	rm -f *.a 
	rm -f *.o 
	rm -f *.d 
	rm -f *.mod 
	rm -f ifc*
	rm -f *.pc
	rm -f *.pcl
	rm -f $(LIB_FILES)/*.a
	rm -f $(MOD_FILES)/*.mod
	rm -f $(OBJ_FILES)/*.o
	rm -f $(OBJ_FILES)/*.d
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make clean
	cd $(ERROR_INFO_CLASS_LOCATION)/Makefile; make clean
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make clean
	cd $(ORDER_OF_ACCURACY_CLASS_LOCATION)/Makefile; make clean

cleanTest:
	rm -f *.FAILED
	rm -f ../Executables/*.FAILED
	rm -f *.PASSED
	rm -f ../Executables/*.PASSED
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make cleanTest
	cd $(ERROR_INFO_CLASS_LOCATION)/Makefile; make cleanTest
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make cleanTest
	cd $(ORDER_OF_ACCURACY_CLASS_LOCATION)/Makefile; make cleanTest

# Dependency definitions

#------------------MessagePassingInterface lib----------------------------

$(LIB_FILES)/libMessagePassingInterface.a: \
  $(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/libMessagePassingInterface.a
	cp $(INTERFACE_WITH_MPI_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp $(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/libMessagePassingInterface.a:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"

MessagePassingInterfaceTest.PASSED: \
  $(LIB_FILES)/libMessagePassingInterface.a \
  $(INTERFACE_WITH_MPI_LOCATION)/Executables/MessagePassingInterfaceTest.PASSED
	cp $(INTERFACE_WITH_MPI_LOCATION)/Executables/MessagePassingInterfaceTest.PASSED .

$(INTERFACE_WITH_MPI_LOCATION)/Executables/MessagePassingInterfaceTest.PASSED:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; \
        make test  \
        "F95=$(F95)"  \
        "F95FLAGS=$(F95FLAGS)"  \
        "F95FLAGS2=$(F95FLAGS2)"  \
        "LDFLAGS = $(LDFLAGS)"  \
        "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
        "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
        "MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
        "TEST_RUN_CMD = $(TEST_RUN_CMD)"

#------------------ErrorInformationClass lib----------------------------

$(LIB_FILES)/libErrorInformationClass.a: \
  $(ERROR_INFO_CLASS_LOCATION)/LibraryFiles/libErrorInformationClass.a
	cp $(ERROR_INFO_CLASS_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp $(ERROR_INFO_CLASS_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(ERROR_INFO_CLASS_LOCATION)/LibraryFiles/libErrorInformationClass.a:
	cd $(ERROR_INFO_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"

ErrorInformationClassTest.PASSED: \
  $(LIB_FILES)/libErrorInformationClass.a \
  $(ERROR_INFO_CLASS_LOCATION)/Executables/ErrorInformationClassTest.PASSED
	cp $(ERROR_INFO_CLASS_LOCATION)/Executables/ErrorInformationClassTest.PASSED .

$(ERROR_INFO_CLASS_LOCATION)/Executables/ErrorInformationClassTest.PASSED:
	cd $(ERROR_INFO_CLASS_LOCATION)/Makefile; \
        make test  \
        "F95=$(F95)"  \
        "F95FLAGS=$(F95FLAGS)"  \
        "F95FLAGS2=$(F95FLAGS2)"  \
        "LDFLAGS = $(LDFLAGS)"  \
        "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
        "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
        "MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
        "TEST_RUN_CMD = $(TEST_RUN_CMD)"

#------------------MPI2DataClassNoErrClass lib----------------------------

$(LIB_FILES)/libMPI2DataClassNoErrClass.a: \
  $(MPI2_DATA_CLASS_LOCATION)/LibraryFiles/libMPI2DataClassNoErrClass.a
	cp -p $(MPI2_DATA_CLASS_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp -p $(MPI2_DATA_CLASS_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(MPI2_DATA_CLASS_LOCATION)/LibraryFiles/libMPI2DataClassNoErrClass.a:
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"

MPI2DataClassNoErrClassTest.PASSED: \
  $(LIB_FILES)/libMPI2DataClassNoErrClass.a \
  $(MPI2_DATA_CLASS_LOCATION)/Executables/MPI2DataClassNoErrClassTest.PASSED
	cp $(MPI2_DATA_CLASS_LOCATION)/Executables/MPI2DataClassNoErrClassTest.PASSED .

$(MPI2_DATA_CLASS_LOCATION)/Executables/MPI2DataClassNoErrClassTest.PASSED:
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; \
        make test  \
        "F95=$(F95)"  \
        "F95FLAGS=$(F95FLAGS)"  \
        "F95FLAGS2=$(F95FLAGS2)"  \
        "LDFLAGS = $(LDFLAGS)"  \
        "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
        "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
        "MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
        "TEST_RUN_CMD = $(TEST_RUN_CMD)"

#------------------OrderOfAccuracyClass lib----------------------------

$(LIB_FILES)/libOrderOfAccuracyClass.a: \
  $(ORDER_OF_ACCURACY_CLASS_LOCATION)/LibraryFiles/libOrderOfAccuracyClass.a
	cp -p $(ORDER_OF_ACCURACY_CLASS_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp -p $(ORDER_OF_ACCURACY_CLASS_LOCATION)/LibraryFiles/*.a $(LIB_FILES)

$(ORDER_OF_ACCURACY_CLASS_LOCATION)/LibraryFiles/libOrderOfAccuracyClass.a:
	cd $(ORDER_OF_ACCURACY_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"

OrderOfAccuracyClassTest.PASSED: \
  $(LIB_FILES)/libOrderOfAccuracyClass.a \
  $(ORDER_OF_ACCURACY_CLASS_LOCATION)/Executables/OrderOfAccuracyClassTest.PASSED
	cp $(ORDER_OF_ACCURACY_CLASS_LOCATION)/Executables/OrderOfAccuracyClassTest.PASSED .

$(ORDER_OF_ACCURACY_CLASS_LOCATION)/Executables/OrderOfAccuracyClassTest.PASSED:
	cd $(ORDER_OF_ACCURACY_CLASS_LOCATION)/Makefile; \
        make test  \
        "F95=$(F95)"  \
        "F95FLAGS=$(F95FLAGS)"  \
        "F95FLAGS2=$(F95FLAGS2)"  \
        "LDFLAGS = $(LDFLAGS)"  \
        "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
        "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
        "MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
        "TEST_RUN_CMD = $(TEST_RUN_CMD)"

#------------------lapack lib----------------------------

$(LIB_FILES)/liblapack.a: \
  $(LAPACK_LOCATION)/liblapack.a
	cp -p $(LAPACK_LOCATION)/*.a $(LIB_FILES)

$(LAPACK_LOCATION)/liblapack.a:
	cd $(LAPACK_LOCATION)/SRC; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"

#--------------------LUSGSClass lib---------------------------------

$(LIB_FILES)/libLUSGSClass.a: \
        $(OBJS) \
        $(LIBS_NEEDED)
	ar r libLUSGSClass.a $(OBJS) $(ERROR_INFO_CLASS_LIB_OBJS) $(MESSAGE_PASSING_INTERFACE_LIB_OBJS) $(MPI2_DATA_CLASS_LIB_OBJS) $(ORDER_OF_ACCURACY_CLASS_LIB_OBJS) $(LAPACK_LIB_OBJS) $(LAPACK_LIB_OBJS2) $(BLAS_LIB_OBJS)
	$(MV_LIB)

$(OBJ_FILES)/LUSGSClass.o:                         \
    $(SRC_FILES)/LUSGSClass.f90                    \
    $(SRC_FILES)/LUSGSClassDef.f90                 \
    $(SRC_FILES)/LUSGSClassRoutinesInviscid4D.f90  \
    $(SRC_FILES)/TauSweep4DInviscid1.f90           \
    $(SRC_FILES)/TauSweep4DInviscid2.f90           \
    $(SRC_FILES)/ZetaSweep4DInviscid1.f90          \
    $(SRC_FILES)/ZetaSweep4DInviscid2.f90          \
    $(SRC_FILES)/EtaSweep4DInviscid1.f90           \
    $(SRC_FILES)/EtaSweep4DInviscid2.f90           \
    $(SRC_FILES)/XiSweep4DInviscid1.f90            \
    $(SRC_FILES)/XiSweep4DInviscid2.f90            \
    $(SRC_FILES)/LUSGSClassRoutinesInviscid3D.f90  \
    $(SRC_FILES)/ZetaSweep3DInviscid1.f90          \
    $(SRC_FILES)/ZetaSweep3DInviscid2.f90          \
    $(SRC_FILES)/EtaSweep3DInviscid1.f90           \
    $(SRC_FILES)/EtaSweep3DInviscid2.f90           \
    $(SRC_FILES)/XiSweep3DInviscid1.f90            \
    $(SRC_FILES)/XiSweep3DInviscid2.f90            \
    $(SRC_FILES)/LUSGSClassRoutinesInviscid2D.f90  \
    $(SRC_FILES)/EtaSweep2DInviscid1.f90           \
    $(SRC_FILES)/EtaSweep2DInviscid2.f90           \
    $(SRC_FILES)/XiSweep2DInviscid1.f90            \
    $(SRC_FILES)/XiSweep2DInviscid2.f90            \
    $(SRC_FILES)/LUSGSClassRoutinesInviscid1D.f90  \
    $(SRC_FILES)/XiSweep1DInviscid1.f90            \
    $(SRC_FILES)/XiSweep1DInviscid2.f90            \
    $(SRC_FILES)/LUSGSClassRoutinesViscous4D.f90   \
    $(SRC_FILES)/TauSweep4DViscous1.f90           \
    $(SRC_FILES)/TauSweep4DViscous2.f90           \
    $(SRC_FILES)/ZetaSweep4DViscous1.f90          \
    $(SRC_FILES)/ZetaSweep4DViscous2.f90          \
    $(SRC_FILES)/EtaSweep4DViscous1.f90           \
    $(SRC_FILES)/EtaSweep4DViscous2.f90           \
    $(SRC_FILES)/XiSweep4DViscous1.f90            \
    $(SRC_FILES)/XiSweep4DViscous2.f90            \
    $(SRC_FILES)/LUSGSClassRoutinesViscous3D.f90   \
    $(SRC_FILES)/ZetaSweep3DViscous1.f90          \
    $(SRC_FILES)/ZetaSweep3DViscous2.f90          \
    $(SRC_FILES)/EtaSweep3DViscous1.f90           \
    $(SRC_FILES)/EtaSweep3DViscous2.f90           \
    $(SRC_FILES)/XiSweep3DViscous1.f90            \
    $(SRC_FILES)/XiSweep3DViscous2.f90            \
    $(SRC_FILES)/LUSGSClassRoutinesViscous2D.f90   \
    $(SRC_FILES)/EtaSweep2DViscous1.f90           \
    $(SRC_FILES)/EtaSweep2DViscous2.f90           \
    $(SRC_FILES)/XiSweep2DViscous1.f90            \
    $(SRC_FILES)/XiSweep2DViscous2.f90            \
    $(SRC_FILES)/LUSGSClassRoutinesViscous1D.f90   \
    $(SRC_FILES)/XiSweep1DViscous1.f90            \
    $(SRC_FILES)/XiSweep1DViscous2.f90            \
    $(LIBS_NEEDED)
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/LUSGSClass.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/LUSGSClassTest.o:            \
    $(SRC_FILES)/LUSGSClassTest.f90       \
    $(OBJ_FILES)/LUSGSClassTest_General.o \
    $(OBJ_FILES)/LUSGSClassTest_LAPACK.o  \
    $(OBJ_FILES)/LUSGSClassTest_1D.o      \
    $(OBJ_FILES)/LUSGSClassTest_2D.o      \
    $(OBJ_FILES)/LUSGSClassTest_3D.o      \
    $(OBJ_FILES)/LUSGSClassTest_4D.o \
    $(OBJ_FILES)/LUSGSClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/LUSGSClassTest.f90
	$(MV_OBJ)

$(OBJ_FILES)/LUSGSClassTest_General.o:           \
    $(SRC_FILES)/LUSGSClassTest_General.f90      \
    $(OBJ_FILES)/LUSGSClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/LUSGSClassTest_General.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/LUSGSClassTest_LAPACK.o:           \
    $(SRC_FILES)/LUSGSClassTest_LAPACK.f90      \
    $(OBJ_FILES)/Check1DData.o                  \
    $(OBJ_FILES)/LUSGSClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/LUSGSClassTest_LAPACK.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/LUSGSClassTest_1D.o:           \
    $(SRC_FILES)/LUSGSClassTest_1D.f90      \
    $(SRC_FILES)/XiTestSweep1DViscous21.f90      \
    $(SRC_FILES)/XiTestSweep1DViscous11.f90      \
    $(SRC_FILES)/XiTestSweep1DViscous22.f90      \
    $(SRC_FILES)/XiTestSweep1DViscous12.f90      \
    $(SRC_FILES)/XiTestSweep1DInviscid21.f90     \
    $(SRC_FILES)/XiTestSweep1DInviscid11.f90     \
    $(SRC_FILES)/XiTestSweep1DInviscid22.f90     \
    $(SRC_FILES)/XiTestSweep1DInviscid12.f90     \
    $(OBJ_FILES)/Check2DData.o              \
    $(OBJ_FILES)/LUSGSClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/LUSGSClassTest_1D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/LUSGSClassTest_2D.o:             \
    $(SRC_FILES)/LUSGSClassTest_2D.f90        \
    $(SRC_FILES)/XiTestSweep2DViscous11.f90   \
    $(SRC_FILES)/EtaTestSweep2DViscous11.f90  \
    $(SRC_FILES)/XiTestSweep2DViscous21.f90   \
    $(SRC_FILES)/EtaTestSweep2DViscous21.f90  \
    $(SRC_FILES)/XiTestSweep2DViscous12.f90   \
    $(SRC_FILES)/EtaTestSweep2DViscous12.f90  \
    $(SRC_FILES)/XiTestSweep2DViscous22.f90   \
    $(SRC_FILES)/EtaTestSweep2DViscous22.f90  \
    $(SRC_FILES)/XiTestSweep2DInviscid11.f90  \
    $(SRC_FILES)/EtaTestSweep2DInviscid11.f90 \
    $(SRC_FILES)/XiTestSweep2DInviscid12.f90  \
    $(SRC_FILES)/EtaTestSweep2DInviscid12.f90 \
    $(SRC_FILES)/XiTestSweep2DInviscid21.f90  \
    $(SRC_FILES)/EtaTestSweep2DInviscid21.f90 \
    $(SRC_FILES)/XiTestSweep2DInviscid22.f90  \
    $(SRC_FILES)/EtaTestSweep2DInviscid22.f90 \
    $(OBJ_FILES)/Check3DData.o                \
    $(OBJ_FILES)/LUSGSClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/LUSGSClassTest_2D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/LUSGSClassTest_3D.o:              \
    $(SRC_FILES)/LUSGSClassTest_3D.f90         \
    $(SRC_FILES)/XiTestSweep3DViscous11.f90    \
    $(SRC_FILES)/EtaTestSweep3DViscous11.f90   \
    $(SRC_FILES)/ZetaTestSweep3DViscous11.f90  \
    $(SRC_FILES)/XiTestSweep3DViscous21.f90    \
    $(SRC_FILES)/EtaTestSweep3DViscous21.f90   \
    $(SRC_FILES)/ZetaTestSweep3DViscous21.f90  \
    $(SRC_FILES)/XiTestSweep3DViscous12.f90    \
    $(SRC_FILES)/EtaTestSweep3DViscous12.f90   \
    $(SRC_FILES)/ZetaTestSweep3DViscous12.f90  \
    $(SRC_FILES)/XiTestSweep3DViscous22.f90    \
    $(SRC_FILES)/EtaTestSweep3DViscous22.f90   \
    $(SRC_FILES)/ZetaTestSweep3DViscous22.f90  \
    $(SRC_FILES)/XiTestSweep3DViscous11.f90    \
    $(SRC_FILES)/EtaTestSweep3DViscous11.f90   \
    $(SRC_FILES)/ZetaTestSweep3DViscous11.f90  \
    $(SRC_FILES)/XiTestSweep3DInviscid21.f90   \
    $(SRC_FILES)/EtaTestSweep3DInviscid21.f90  \
    $(SRC_FILES)/ZetaTestSweep3DInviscid21.f90 \
    $(SRC_FILES)/XiTestSweep3DInviscid12.f90   \
    $(SRC_FILES)/EtaTestSweep3DInviscid12.f90  \
    $(SRC_FILES)/ZetaTestSweep3DInviscid12.f90 \
    $(SRC_FILES)/XiTestSweep3DInviscid22.f90   \
    $(SRC_FILES)/EtaTestSweep3DInviscid22.f90  \
    $(SRC_FILES)/ZetaTestSweep3DInviscid22.f90 \
    $(OBJ_FILES)/Check4DData.o                 \
    $(OBJ_FILES)/LUSGSClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/LUSGSClassTest_3D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/LUSGSClassTest_4D.o:               \
    $(SRC_FILES)/LUSGSClassTest_4D.f90          \
    $(SRC_FILES)/XiTestSweep4DViscous11.f90     \
    $(SRC_FILES)/EtaTestSweep4DViscous11.f90    \
    $(SRC_FILES)/ZetaTestSweep4DViscous11.f90   \
    $(SRC_FILES)/TauTestSweep4DViscous11.f90    \
    $(SRC_FILES)/XiTestSweep4DViscous21.f90     \
    $(SRC_FILES)/EtaTestSweep4DViscous21.f90    \
    $(SRC_FILES)/ZetaTestSweep4DViscous21.f90   \
    $(SRC_FILES)/TauTestSweep4DViscous21.f90    \
    $(SRC_FILES)/XiTestSweep4DViscous12.f90     \
    $(SRC_FILES)/EtaTestSweep4DViscous12.f90    \
    $(SRC_FILES)/ZetaTestSweep4DViscous12.f90   \
    $(SRC_FILES)/TauTestSweep4DViscous12.f90    \
    $(SRC_FILES)/XiTestSweep4DViscous22.f90     \
    $(SRC_FILES)/EtaTestSweep4DViscous22.f90    \
    $(SRC_FILES)/ZetaTestSweep4DViscous22.f90   \
    $(SRC_FILES)/TauTestSweep4DViscous22.f90    \
    $(SRC_FILES)/XiTestSweep4DInviscid11.f90    \
    $(SRC_FILES)/EtaTestSweep4DInviscid11.f90   \
    $(SRC_FILES)/ZetaTestSweep4DInviscid11.f90  \
    $(SRC_FILES)/TauTestSweep4DInviscid11.f90   \
    $(SRC_FILES)/XiTestSweep4DInviscid21.f90    \
    $(SRC_FILES)/EtaTestSweep4DInviscid21.f90   \
    $(SRC_FILES)/ZetaTestSweep4DInviscid21.f90  \
    $(SRC_FILES)/TauTestSweep4DInviscid21.f90   \
    $(SRC_FILES)/XiTestSweep4DInviscid12.f90    \
    $(SRC_FILES)/EtaTestSweep4DInviscid12.f90   \
    $(SRC_FILES)/ZetaTestSweep4DInviscid12.f90  \
    $(SRC_FILES)/TauTestSweep4DInviscid12.f90   \
    $(SRC_FILES)/XiTestSweep4DInviscid22.f90    \
    $(SRC_FILES)/EtaTestSweep4DInviscid22.f90   \
    $(SRC_FILES)/ZetaTestSweep4DInviscid22.f90  \
    $(SRC_FILES)/TauTestSweep4DInviscid22.f90   \
    $(OBJ_FILES)/Check5DData.o                  \
    $(OBJ_FILES)/LUSGSClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/LUSGSClassTest_4D.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check1DData.o:           \
    $(SRC_FILES)/Check1DData.f90    
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check1DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check2DData.o:           \
    $(SRC_FILES)/Check2DData.f90    
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check2DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check3DData.o:           \
    $(SRC_FILES)/Check3DData.f90    
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check3DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check4DData.o:           \
    $(SRC_FILES)/Check4DData.f90    
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check4DData.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/Check5DData.o:           \
    $(SRC_FILES)/Check5DData.f90    
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/Check5DData.f90
	$(MV_OBJ); $(MV_MOD)



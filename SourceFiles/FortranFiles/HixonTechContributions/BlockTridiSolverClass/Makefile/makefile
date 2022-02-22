### MAKEFILE DEFINITIONS ###

GFORT = mpif90

GFORTFLAGS_OPT = -c -O3
GFORTFLAGS2_OPT = -c -O3
GFORTLDFLAGS_OPT = -O3

GFORTFLAGS_DEBUG = -c -O0 -Wall -Werror -Wline-truncation -Wuninitialized -Wconversion -Wsurprising -std=f2008 -fbounds-check -Warray-temporaries -Wconversion-extra -Wintrinsics-std -fcheck=all
GFORTFLAGS2_DEBUG = -c -O0 -Wall -Werror -Wline-truncation -Wuninitialized -Wconversion -Wsurprising -std=f2008 -fbounds-check -Warray-temporaries -Wconversion-extra -Wintrinsics-std -fcheck=all
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

CMD = BlockTridiSolverClass.exe

# define global paths

INTERFACE_WITH_MPI_LOCATION = ../../InterfaceWithMPI
ERROR_INFO_CLASS_LOCATION = ../../ErrorInformationClass
MPI2_DATA_CLASS_LOCATION = ../../MPI2DataClass

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

MV_LIB = mv libBlockTridiSolverClass.a $(LIB_FILES)
MV_LIB_MOD = mv *.mod $(MOD_FILES)
MV_OBJ = mv *.o $(OBJ_FILES)
MV_MOD = mv *.mod $(MOD_FILES)
MV_EXE = mv $(CMD) $(EXE_FILES)

TEST_SUCCESS = BlockTridiSolverClassTest.PASSED

BLOCK_TRIDI_SOLVER_CLASS_LIBS =            \
    $(LIB_FILES)/BlockTridiSolverClass.a  \

BLOCK_TRIDI_SOLVER_CLASS_OBJS =              \
    $(OBJ_FILES)/BlockTridiSolverClass.o 

LIBS_NEEDED = \
 $(LIB_FILES)/libMessagePassingInterface.a \
 $(LIB_FILES)/libErrorInformationClass.a \
 $(LIB_FILES)/libMPI2DataClassNoErrClass.a

OBJS = \
 $(BLOCK_TRIDI_SOLVER_CLASS_OBJS)

TEST_OBJS =                               \
 $(OBJ_FILES)/BlockTridiSolverClassTest.o    \
 $(OBJ_FILES)/BlockTridiSolverClassTest_General.o \
 $(OBJ_FILES)/BlockTridiSolverClassTest_1D.o \
 $(OBJ_FILES)/BlockTridiSolverClassTest_2D.o \
 $(OBJ_FILES)/BlockTridiSolverClassTest_3D.o \
 $(OBJ_FILES)/BlockTridiSolverClassTest_4D.o

PREVIOUS_TESTS = \
 MessagePassingInterfaceTest.PASSED \
 ErrorInformationClassTest.PASSED   \
 MPI2DataClassNoErrClassTest.PASSED

LIBS = \
 -lMPI2DataClassNoErrClass   \
 -lMessagePassingInterface   \
 -lErrorInformationClass     \
 -lBlockTridiSolverClass

MODS = \
 -I$(MOD_FILES)

MESSAGE_PASSING_INTERFACE_LIB_OBJS = \
   $(INTERFACE_WITH_MPI_LOCATION)/ObjectFiles/*.o

ERROR_INFO_CLASS_LIB_OBJS = \
   $(ERROR_INFO_CLASS_LOCATION)/ObjectFiles/*.o

MPI2_DATA_CLASS_LIB_OBJS = \
   $(MPI2_DATA_CLASS_LOCATION)/ObjectFiles/*.o

#------Commands---------------------

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

# Linkage rule

checkLibs:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	cd $(ERROR_INFO_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)" "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)" "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)" "MPI_LIBRARIES = $(MPI_LIBRARIES)"
	make all


all: $(LIB_FILES)/libBlockTridiSolverClass.a
	@echo " "
	@echo "-------------------------------------------------------- "
	@echo "-   Hixon Technologies BlockTridiSolverClass routines     - "
	@echo "-    Copyright 2020 by Hixon Technologies, LLC         - "
	@echo "-    Used with permission.                             - "
	@echo "-    These routines come with no warranty.             - "
	@echo "-                                                      - "
	@echo "-   Contact:  Ray Hixon, Hixon Technologies            - "
	@echo "-             fshixon@yahoo.com                        - "
	@echo "-------------------------------------------------------- "
	@echo " "

test: $(TEST_SUCCESS)

$(TEST_SUCCESS):  \
      $(PREVIOUS_TESTS) \
      $(LIB_FILES)/libBlockTridiSolverClass.a \
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

cleanTest:
	rm -f *.FAILED
	rm -f ../Executables/*.FAILED
	rm -f *.PASSED
	rm -f ../Executables/*.PASSED
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make cleanTest
	cd $(ERROR_INFO_CLASS_LOCATION)/Makefile; make cleanTest
	cd $(MPI2_DATA_CLASS_LOCATION)/Makefile; make cleanTest

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

#--------------------BlockTridiSolverClass lib---------------------------------

$(LIB_FILES)/libBlockTridiSolverClass.a: \
        $(OBJS) \
        $(LIBS_NEEDED)
	ar r libBlockTridiSolverClass.a $(OBJS) $(ERROR_INFO_CLASS_LIB_OBJS) $(MESSAGE_PASSING_INTERFACE_LIB_OBJS) $(MPI2_DATA_CLASS_LIB_OBJS) $(MPIIO_DATA_CLASS_LIB_OBJS) $(POINT4D_CLASS_LIB_OBJS) $(BOUNDING_BOX_CLASS_LIB_OBJS)
	$(MV_LIB)

$(OBJ_FILES)/BlockTridiSolverClass.o:           \
    $(SRC_FILES)/BlockTridiSolverClass.f90      \
    $(SRC_FILES)/BlockTridiSolver1D.f90         \
    $(SRC_FILES)/BlockTridiSolver2D.f90         \
    $(SRC_FILES)/BlockTridiSolver3D.f90         \
    $(SRC_FILES)/BlockTridiSolver4D.f90         \
    $(LIBS_NEEDED)
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/BlockTridiSolverClass.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/BlockTridiSolverClassTest.o:            \
    $(SRC_FILES)/BlockTridiSolverClassTest.f90       \
    $(OBJ_FILES)/BlockTridiSolverClassTest_General.o \
    $(OBJ_FILES)/BlockTridiSolverClassTest_1D.o      \
    $(OBJ_FILES)/BlockTridiSolverClassTest_2D.o      \
    $(OBJ_FILES)/BlockTridiSolverClassTest_3D.o      \
    $(OBJ_FILES)/BlockTridiSolverClassTest_4D.o      \
    $(OBJ_FILES)/BlockTridiSolverClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/BlockTridiSolverClassTest.f90
	$(MV_OBJ)

$(OBJ_FILES)/BlockTridiSolverClassTest_General.o:            \
    $(SRC_FILES)/BlockTridiSolverClassTest_General.f90       \
    $(OBJ_FILES)/BlockTridiSolverClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/BlockTridiSolverClassTest_General.f90
	$(MV_OBJ)

$(OBJ_FILES)/BlockTridiSolverClassTest_1D.o:            \
    $(SRC_FILES)/BlockTridiSolverClassTest_1D.f90       \
    $(OBJ_FILES)/BlockTridiSolverClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/BlockTridiSolverClassTest_1D.f90
	$(MV_OBJ)

$(OBJ_FILES)/BlockTridiSolverClassTest_2D.o:            \
    $(SRC_FILES)/BlockTridiSolverClassTest_2D.f90       \
    $(OBJ_FILES)/BlockTridiSolverClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/BlockTridiSolverClassTest_2D.f90
	$(MV_OBJ)

$(OBJ_FILES)/BlockTridiSolverClassTest_3D.o:            \
    $(SRC_FILES)/BlockTridiSolverClassTest_3D.f90       \
    $(OBJ_FILES)/BlockTridiSolverClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/BlockTridiSolverClassTest_3D.f90
	$(MV_OBJ)

$(OBJ_FILES)/BlockTridiSolverClassTest_4D.o:            \
    $(SRC_FILES)/BlockTridiSolverClassTest_4D.f90       \
    $(OBJ_FILES)/BlockTridiSolverClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/BlockTridiSolverClassTest_4D.f90
	$(MV_OBJ)


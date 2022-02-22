### MAKEFILE DEFINITIONS ###

F95_IFC      = ifort
F95FLAGS_IFC = -c  -O0 -CU -CB
#F95FLAGS_IFC = -c  -O3 -pad -align -prec_div
LDFLAGS_IFC  = -static

F95_G95      = mpif90
F95FLAGS_G95 = -c -O3
#F95FLAGS_G95 = -c -O0 -Wall -Werror -Wline-truncation -Wmissing-intent -Wuninitialized -Wprecision-loss -fbounds-check
#F95FLAGS2_G95 = -c -O0 -Wall -Werror -Wline-truncation -Wmissing-intent -Wuninitialized -Wprecision-loss -fbounds-check
LDFLAGS_G95  =

F95_GFORTRAN      = mpif90
#F95FLAGS_GFORTRAN = -c -O3
#F95FLAGS2_GFORTRAN = -c -O3
F95FLAGS_GFORTRAN = -c -O0 -Wall -Werror -Wline-truncation -Wuninitialized -Wconversion \
                  -Wintrinsics-std -fcheck=all -Wsurprising -fbounds-check -std=f2008  \
                  -Wconversion-extra -Warray-temporaries -fcheck=all
F95FLAGS2_GFORTRAN = -c -O0 -Wall -Werror -Wline-truncation -Wuninitialized -Wconversion \
                  -Wintrinsics-std -fcheck=all -Wsurprising -fbounds-check -std=f2008  \
                  -Wconversion-extra -Warray-temporaries -fcheck=all
LDFLAGS_GFORTRAN  =

F95_SICOR      = mpif90
F95FLAGS_SICOR = -c -O3 -64
F95FLAGS2_SICOR = -c -O0 -64
LDFLAGS_SICOR  = -v -64

F95      = $(F95_GFORTRAN)
F95FLAGS = $(F95FLAGS_GFORTRAN)
F95FLAGS2 = $(F95FLAGS2_GFORTRAN)
LDFLAGS  = $(LDFLAGS_GFORTRAN)

# define locations for files

LIB_FILES = ../LibraryFiles
MOD_FILES = ../ModuleFiles
OBJ_FILES = ../ObjectFiles
EXE_FILES = ../Executables
SRC_FILES = ../SourceCode

CMD = ErrorInformationClassTest.exe

# define global paths

#INTERFACE_WITH_MPI_LOCATION = ../../../InterfaceWithMPI
INTERFACE_WITH_MPI_LOCATION = ../../InterfaceWithMPI
CURRENT_LOCATION = ../../INTERFACE_WITH_MPI_LOCATION = ../../InterfaceWithMPI

# MPICH2 definitions

MPICH2_INCLUDE_LOCATION = /usr/local/include
MPICH2_LIBRARY_LOCATION = /usr/local/lib
MPICH2_LIBRARIES =
#MPICH2_LIBRARIES = -lmpi_f90 \
                   -lmpi_f77       \
                   -lmpi       \
                   -lopen-rte       \
                   -lopen-pal       \
                   -ldl        \
                   -lnsl      \
                   -lutil      \
                   -lm      \
                   -pthread

MPICH2_TEST_RUN_CMD = mpiexec -n 3
MPICH2_VALGRIND_TEST_RUN_CMD_LOCAL = mpiexec -n 2 valgrind --tool=memcheck --leak-check=yes --track-origins=yes

# OpenMPI definitions

OPENMPI_INCLUDE_LOCATION = /usr/local/include
OPENMPI_LIBRARY_LOCATION = /usr/local/library
OPENMPI_LIBRARIES = -llamf77mpi \
                   -lmpi       \
                   -llam       \
                   -lutil      \
                   -ldl        \
                   -pthread

OPEN_TEST_RUN_CMD = mpirun -np 2 -s n0

# LAM MPI definitions

LAMMPI_INCLUDE_LOCATION = /usr/local/include
LAMMPI_LIBRARY_LOCATION = /usr/local/library
LAMMPI_LIBRARIES = -llamf77mpi \
                   -lmpi       \
                   -llam       \
                   -lutil      \
                   -ldl        \
                   -pthread

LAM_TEST_RUN_CMD = mpirun -np 2 -s n0

SICORMPI_INCLUDE_LOCATION = /usr/include/gentoo-multilib/n64
SICORMPI_LIBRARY_LOCATION = /usr/lib
SICORMPI_LIBRARIES = -lscmpi_debug

SICOR_TEST_RUN_CMD = srun -p sca -n 2

DEFAULT_MPI_INCLUDE_LOCATION = ../
DEFAULT_MPI_LIBRARY_LOCATION = ../
DEFAULT_MPI_LIBRARIES =
DEFAULT_MPI_TEST_RUN_CMD = mpiexec -np 2
DEFAULT_MPI_TEST_RUN_CMD_LOCAL = mpiexec -np 2

MPI_INCLUDE_LOCATION = $(DEFAULT_MPI_INCLUDE_LOCATION)
MPI_LIBRARY_LOCATION = $(DEFAULT_MPI_LIBRARY_LOCATION)
MPI_LIBRARIES = $(DEFAULT_MPI_LIBRARIES)
TEST_RUN_CMD = $(DEFAULT_MPI_TEST_RUN_CMD)
VALGRIND_TEST_RUN_CMD_LOCAL = $(MPICH2_VALGRIND_TEST_RUN_CMD_LOCAL)

# define commands

MV_LIB = mv libErrorInformationClass.a $(LIB_FILES)
MV_LIB_MOD = mv *.mod $(MOD_FILES)
MV_OBJ = mv *.o $(OBJ_FILES)
MV_MOD = mv *.mod $(MOD_FILES)
MV_EXE = mv $(CMD) $(EXE_FILES)

TEST_SUCCESS = ErrorInformationClassTest.PASSED
VALGRIND_TEST_SUCCESS = ErrorInformationClassValgrindTest.PASSED

ERROR_INFO_CLASS_LIBS =            \
    $(LIB_FILES)/ErrorInformationClass.a  \

ERROR_INFO_CLASS_OBJS =            \
    $(OBJ_FILES)/ErrorInformationClass.o

LIBS_NEEDED = \
 $(LIB_FILES)/libMessagePassingInterface.a

OBJS = \
 $(ERROR_INFO_CLASS_OBJS)

TEST_OBJS = \
 $(OBJ_FILES)/ErrorInformationClassTest.o

PREVIOUS_TESTS = \
 MessagePassingInterfaceTest.PASSED

PREVIOUS_VALGRIND_TESTS = \
 MessagePassingInterfaceValgrindTest.PASSED

LIBS = \
 -lErrorInformationClass

MODS = \
 -I$(MOD_FILES)

MESSAGE_PASSING_INTERFACE_LIB_OBJS = \
   $(INTERFACE_WITH_MPI_LOCATION)/ObjectFiles/*.o

# Linkage rule

checkLibs:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	make all

all: $(LIB_FILES)/libErrorInformationClass.a
	@echo " "
	@echo "-------------------------------------------------------- "
	@echo "-   Hixon Technologies ErrorInformationClass routines  - "
	@echo "-    Copyright 2012 by Hixon Technologies, LLC         - "
	@echo "-    Used with permission.                             - "
	@echo "-    These routines come with no warranty.             - "
	@echo "-                                                      - "
	@echo "-   Contact:  Ray Hixon, Hixon Technologies            - "
	@echo "-             rhixon@wideopenwest.com                  - "
	@echo "-------------------------------------------------------- "
	@echo " "

$(TEST_SUCCESS):  \
      $(PREVIOUS_TESTS) \
      $(LIB_FILES)/libErrorInformationClass.a \
      $(TEST_OBJS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(MODS) $(LIBS) -L$(LIB_FILES) -L$(MPI_LIBRARY_LOCATION) $(MPI_LIBRARIES)
	$(MV_EXE)
	cd $(EXE_FILES); $(TEST_RUN_CMD) ./$(CMD)
	cp $(EXE_FILES)/$(TEST_SUCCESS) .

test: $(TEST_SUCCESS)

$(VALGRIND_TEST_SUCCESS):  \
      $(PREVIOUS_VALGRIND_TESTS) \
      $(LIB_FILES)/libErrorInformationClass.a \
      $(TEST_OBJS)
	$(F95) -o $(CMD) $(TEST_OBJS) $(MODS) $(LIBS) -L$(LIB_FILES) -L$(MPI_LIBRARY_LOCATION) $(MPI_LIBRARIES)
	$(MV_EXE)
	cd $(EXE_FILES); $(VALGRIND_TEST_RUN_CMD_LOCAL) ./$(CMD) &> outValErrorInformationClassTest.txt
	cp $(EXE_FILES)/$(TEST_SUCCESS) ./$(VALGRIND_TEST_SUCCESS)

valgrindTest: $(VALGRIND_TEST_SUCCESS)

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

cleanTest:
	rm -f *.FAILED
	rm -f ../Executables/*.FAILED
	rm -f *.PASSED
	rm -f ../Executables/*.PASSED
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make cleanTest

# Dependency definitions

#------------------MessagePassingInterface lib----------------------------

$(LIB_FILES)/libMessagePassingInterface.a: \
  $(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/libMessagePassingInterface.a
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"
	cp -p $(INTERFACE_WITH_MPI_LOCATION)/ModuleFiles/*.mod $(MOD_FILES)
	cp -p $(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/libMessagePassingInterface.a $(LIB_FILES)

$(INTERFACE_WITH_MPI_LOCATION)/LibraryFiles/libMessagePassingInterface.a:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; make "F95=$(F95)" "F95FLAGS=$(F95FLAGS)" "F95FLAGS2=$(F95FLAGS2)" "LDFLAGS = $(LDFLAGS)"

MessagePassingInterfaceTest.PASSED: \
  $(LIB_FILES)/libMessagePassingInterface.a \
  $(INTERFACE_WITH_MPI_LOCATION)/Executables/MessagePassingInterfaceTest.PASSED
	cp -u $(INTERFACE_WITH_MPI_LOCATION)/Executables/MessagePassingInterfaceTest.PASSED .

MessagePassingInterfaceValgrindTest.PASSED: \
  $(LIB_FILES)/libMessagePassingInterface.a \
  $(INTERFACE_WITH_MPI_LOCATION)/Makefile/MessagePassingInterfaceValgrindTest.PASSED
	cp -u $(INTERFACE_WITH_MPI_LOCATION)/Makefile/MessagePassingInterfaceValgrindTest.PASSED .

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

$(INTERFACE_WITH_MPI_LOCATION)/Executables/MessagePassingInterfaceValgrindTest.PASSED:
	cd $(INTERFACE_WITH_MPI_LOCATION)/Makefile; \
        make valgrindTest  \
        "F95=$(F95)"  \
        "F95FLAGS=$(F95FLAGS)"  \
        "F95FLAGS2=$(F95FLAGS2)"  \
        "LDFLAGS = $(LDFLAGS)"  \
        "MPI_INCLUDE_LOCATION = $(MPI_INCLUDE_LOCATION)"  \
        "MPI_LIBRARY_LOCATION = $(MPI_LIBRARY_LOCATION)"  \
        "MPI_LIBRARIES = $(MPI_LIBRARIES)"  \
        "TEST_RUN_CMD = $(VALGRIND_TEST_RUN_CMD_LOCAL)"

#--------------------ErrorInformationClass lib---------------------------------

$(LIB_FILES)/libErrorInformationClass.a: \
        $(OBJS) \
        $(LIBS_NEEDED)
	ar r libErrorInformationClass.a $(OBJS) $(MESSAGE_PASSING_INTERFACE_LIB_OBJS)
	$(MV_LIB)

$(OBJ_FILES)/ErrorInformationClass.o:           \
    $(SRC_FILES)/ErrorInformationClass.f90      \
    $(LIBS_NEEDED)
	$(F95) $(F95FLAGS) $(MODS) $(SRC_FILES)/ErrorInformationClass.f90
	$(MV_OBJ); $(MV_MOD)

$(OBJ_FILES)/ErrorInformationClassTest.o:                \
    $(SRC_FILES)/ErrorInformationClassTest.f90      \
    $(OBJ_FILES)/ErrorInformationClass.o
	$(F95) $(F95FLAGS2) $(MODS) $(SRC_FILES)/ErrorInformationClassTest.f90
	$(MV_OBJ)

